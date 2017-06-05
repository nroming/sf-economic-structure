if(!file.exists("output/common/df.rda")){
  message("Reading and preparing data from scratch. This takes a few seconds.")

  if(!dir.exists("output/common")){
    dir.create("output/common", recursive = TRUE, showWarnings = FALSE)
  }

  # include data that was previously taken from the IDA package
  source("R/IDA_independency_data.R")

  saveRDS(idata_n, file = "output/common/idata.rda")

  wdi <- filter(idata_n, source_id == "WDI_2015", variable %in% vars,
                unit %in% c("bn USD2005/yr", "million", "km2", "1"))

  ssp_gdp <- filter(idata_n, source_id == "SSP", model == "OECD Env-Growth",
                    variable == "GDP",
                    unit == "bn USD2005/yr",
                    temporal >= 2010)

  ssp_pop <- filter(idata_n, source_id == "SSP", model == "IIASA-WiC POP",
                    variable == "Population",
                    unit == "million",
                    temporal >= 2010)

  ssp_urb <- filter(idata_n, source_id == "SSP", model == "NCAR",
                    variable == "Population|Urban|Share",
                    unit == "%",
                    temporal >= 2010) %>%
    mutate(value = value/100,
           unit = "1")

  ssp_urb <- interpolate_missing_years(ssp_urb)

  df <- rbind(wdi, ssp_gdp, ssp_pop, ssp_urb) # combine dataframe for renaming

  rm(wdi, ssp_pop, ssp_gdp, ssp_urb) # clean up

  # rename variables
  for (var in vars){
    df <- rename_var(df, var, names(vars)[vars == var])
  }

  # store units
  units <- distinct(df, variable, unit)

  df <- dcast(df, scenario + spatial + temporal ~ variable)

  # compute current account levels
  # this is necessary since the current account levels in WDI are given in
  # current US$
  df <- mutate(df, ca = ca_share * gdp)

  # compute net exports
  df <- mutate(df, nx = ex - im)

  # compute taxes (on value added from industry and services)
  df <- mutate(df, tax = tax_share * (va_ind + va_ser))

  # split up data into separate dataframes for historic and scenario data
  df_hist <- filter(df, scenario == "history")
  df_scen <- filter(df, scenario != "history")

  # attach information about country area
  country_area <- filter(df_hist, temporal == 2010) %>%
    select(spatial, area) %>%
    distinct() %>%
    na.omit()

  # drop old area column (only NAs)
  df_scen <- select(df_scen, -area)

  # attach country area information
  df_scen <- inner_join(df_scen, country_area, by = c("spatial"))

  # limit scenario data to countries that are present in the historic data
  df_scen <- filter(df_scen, !(spatial %in% setequal(unique(df_scen$spatial),
                                                   unique(df_hist$spatial))))

  # recombine data
  df <- rbind(df_hist, df_scen)

  # compute per capita values
  df <- mutate(df, gdp_pc = gdp / pop,
               va_agr_pc = va_agr / pop,
               va_ind_pc = va_ind / pop,
               va_man_pc = va_man / pop,
               va_ser_pc = va_ser / pop,
               va_agrind_pc = va_agr_pc + va_ind_pc,
               pop_dens = pop / area,
               ca_pc = ca / pop,
               nx_pc = nx / pop,
               nx_pc_share = nx_pc / gdp_pc,
               tax_pc = tax / pop
  )

  # add scenario for net exports ----
  # find last observed value of nx
  df_nx <- select(df, scenario, spatial, temporal, nx_pc_share) %>%
    group_by(spatial) %>%
    filter(!is.na(nx_pc_share)) %>%
    filter(temporal == max(temporal, na.rm = TRUE)) %>%
    ungroup()

  # preallocate dataframe for scenario
  df_nx_scen <- data.frame()

  # include the necessary information
  for (ssp in c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")){
      tmp1 <- mutate(df_nx, temporal = 2100,
                       nx_pc_share = 0,
                       scenario = ssp)

      # create historic data point for each SSP for later grouping and
      # interpolation
      tmp2 <- mutate(df_nx, scenario = ssp)

      tmp1 <- rbind(tmp1, tmp2)

      df_nx_scen <- rbind(df_nx_scen, tmp1)

      rm(tmp1, tmp2)
  }

  # interpolation of net exports
  # include intervening years
  years_iv <- expand.grid(list(temporal = 2005:2100,
                               spatial = unique(df_nx_scen$spatial),
                               scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")))

  # find the actual year of the last historical observation for each country
  years_hist_last <- select(df_nx, spatial, temporal) %>%
    rename(last = temporal)

  years_iv <- inner_join(years_iv, years_hist_last, by = "spatial") %>%
    group_by(spatial) %>%
    filter(temporal >= last) %>%
    select(-last)

  # join with actual data to get explicit gaps
  df_nx_scen <- full_join(df_nx_scen, years_iv, by = c("scenario", "spatial", "temporal"))

  # interpolation
  df_nx_scen <- group_by(df_nx_scen, scenario, spatial) %>%
    arrange(temporal) %>%
    mutate(nx_pc_share = na.approx(nx_pc_share))

  # limit it to SSP scenario data years
  # get miminum year for scenario data
  temp_min_scen <-filter(df, scenario != "history") %>% select(temporal) %>% min()

  # remove data before that year
  df_nx_scen <- filter(df_nx_scen, temporal >= temp_min_scen)

  # rename column before joining since there is already data present
  df_nx_scen <- rename(df_nx_scen, nx_pc_share_scen = nx_pc_share)

  # and merge with main data
  df <- full_join(df, df_nx_scen, by = c("scenario", "spatial", "temporal"))

  # use interpolated data only of no historical data is available
  df <- mutate(df, nx_pc_share = ifelse(is.na(nx_pc_share), nx_pc_share_scen, nx_pc_share))

  # calculate growth rates
  df <- group_by(df, scenario, spatial) %>%
    mutate(va_agr_pc_gr = lag(va_agr_pc, n = 0, order_by = temporal) /
             lag(va_agr_pc, n = 1, order_by = temporal) - 1,
           va_ind_pc_gr = lag(va_ind_pc, n = 0, order_by = temporal) /
             lag(va_ind_pc, n = 1, order_by = temporal) - 1,
           va_ser_pc_gr = lag(va_ser_pc, n = 0, order_by = temporal) /
             lag(va_ser_pc, n = 1, order_by = temporal) - 1,
           gdp_pc_gr = lag(gdp_pc, n = 0, order_by = temporal) /
             lag(gdp_pc, n = 1, order_by = temporal) - 1) %>%
    ungroup()

  # add recession dummy: 1 in case of a recession
  df$recession <- NA
  df[df$gdp_pc_gr > 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 0
  df[df$gdp_pc_gr <= 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 1

  # compute ratio of country level GDP per capita to global GDP per capita as a
  # measure of convergence
  df <- group_by(df, scenario, temporal) %>%
    mutate(gdp_pc_glob = sum(gdp, na.rm = TRUE)/sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ratio_gdp_pc2glob = gdp_pc/gdp_pc_glob)

  # regional mapping
  map_region <- read.csv("data/regions_definition.csv") %>%
  select(ISO, reg11) %>%
  filter(!(reg11 %in% c("INTship", "INTair", "glob"))) %>%
  rename(spatial = ISO)

  # write data to disk
  saveRDS(df, "output/common/df.rda")
  saveRDS(map_region, "output/common/map_region.rda")
  saveRDS(units, "output/common/units.rda")
} else {
  message(c("Reading previously saved prepared data to save time. Delete 'output'
  directory to read and prepare data from scratch."))
  df <- readRDS("output/common/df.rda")
  map_region <- readRDS("output/common/map_region.rda")
  idata_n <- readRDS("output/common/idata.rda")
  units <- readRDS("output/common/units.rda")
}

# create some common plots ----
if(!dir.exists("output/common/figures")){
  dir.create("output/common/figures", recursive = TRUE, showWarnings = FALSE)
}

gdppc_comp <- filter(df, temporal %in% 2010:2015, scenario %in% c("history", "SSP2")) %>%
  select(scenario, spatial, temporal, gdp_pc)

gdppc_comp <- dcast(gdppc_comp, spatial + temporal ~ scenario, value.var = "gdp_pc")

# interpolation of missing values
gdppc_comp <- group_by(gdppc_comp, spatial) %>%
  mutate(SSP2 = na.approx(SSP2, along = temporal)) %>%
  ungroup() %>%
  filter(temporal %in% c(2010, 2014))

gdppc_comp <- mutate(gdppc_comp, deviation = abs((SSP2 - history)/history),
                    label = NA,
                    spatial = as.character(spatial)) %>%
  filter(!is.na(deviation))

gdppc_comp[gdppc_comp$deviation > 0.1 & gdppc_comp$temporal == 2010, "label"] <-
  gdppc_comp[gdppc_comp$deviation > 0.1 & gdppc_comp$temporal == 2010, "spatial"]

# compute the total error
err_tot <- group_by(gdppc_comp, temporal) %>%
  summarize(dev_avg = mean(deviation, na.rm = TRUE))

# log transformation
gdppc_comp <- mutate(gdppc_comp, history = log(history),
                     SSP2 = log(SSP2))

# transform year to character
gdppc_comp <- mutate(gdppc_comp, temporal = as.character(temporal))

ggplot() +
  geom_line(data = gdppc_comp, aes(x = history, y = SSP2, group = spatial), size = 0.3) +
  geom_text(data = gdppc_comp, aes(x = history, y = SSP2, label = label, hjust = -0.2), size = 1.4) +
  geom_abline(intercept = 0, slope = 1, size = 0.1) +
  theme_light(base_size = 11)
ggsave("output/common/figures/compare_GDPpC_data.png", width = 12, height = 8, units = "cm")
