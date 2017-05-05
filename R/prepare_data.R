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

  df <- rbind(wdi, ssp_gdp, ssp_pop) # combine dataframe for renaming

  rm(wdi, ssp_pop, ssp_gdp) # clean up

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
               tax_pc = tax / pop
  )

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

# create some common plots ---
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
