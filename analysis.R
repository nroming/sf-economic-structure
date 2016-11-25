library(IDA)
library(reshape2)
library(ggplot2)

rm(list = ls())

# set warning level
options(warn = -1)

# disable scientific notation
options(scipen = 999)

# create output directories
if(!dir.exists("plots")) dir.create("plots", showWarnings = FALSE)
if(!dir.exists("cache")) dir.create("cache", showWarnings = FALSE)

# prepare data ----
# vector of variables
# ATTENTION: when adding variables here, you have to make sure that the
# corresponding units are included in the filtering of 'idata' below
vars <- c("gdp" = "GDP",
          "va_agr" = "Value Added|Agriculture",
          "va_ind" = "Value Added|Industry",
          "va_man" = "Value Added|Manufacturing",
          "va_ser" = "Value Added|Services",
          "pop" = "Population",
          "area" = "Area")

# G20 memberstates (ATTENTION: other EU members not yet present, also: no
# sectoral data present for Canada)
g20 <- c("DEU", "ARG", "AUS", "BRA", "CHN", "FRA", "GBR", "IND", "IDN", "ITA",
         "JPN", "CAN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "USA")

if(!file.exists(("cache/df.rda"))){
  message("Reading and preparing data from scratch. This takes a few seconds.")
  wdi <- filter(idata, source_id == "WDI_2015", variable %in% vars,
                unit %in% c("bn USD2005/yr", "million", "km2"))

  ssp_gdp <- filter(idata, source_id == "SSP", model == "OECD Env-Growth",
                    variable == "GDP",
                    unit == "bn USD2005/yr",
                    temporal >= 2010)

  ssp_gdp <- interpolate_missing_years(ssp_gdp)

  ssp_pop <- filter(idata, source_id == "SSP", model == "IIASA-WiC POP",
                    variable == "Population",
                    unit == "million",
                    temporal >= 2010)

  ssp_pop <- interpolate_missing_years(ssp_pop)

  df <- rbind(wdi, ssp_gdp, ssp_pop) # combine dataframe for renaming

  # df <- filter(df, spatial %in% g20)

  rm(wdi, ssp_pop, ssp_gdp) # clean up

  # rename variables
  for (var in vars){
    df <- rename_var(df, var, names(vars)[vars == var])
  }

  # store units
  units <- distinct(df, variable, unit)

  df <- dcast(df, scenario + spatial + temporal ~ variable)

  # compute per capita values
  df <- mutate(df, gdp_pc = gdp / pop,
               va_agr_pc = va_agr / pop,
               va_ind_pc = va_ind / pop,
               va_man_pc = va_man / pop,
               va_ser_pc = va_ser / pop,
               va_agrind_pc = va_agr_pc + va_ind_pc,
               pop_dens = pop / area
  )

  # calculate growth rates
  df <- group_by(df, scenario, spatial) %>%
    mutate(va_agr_pc_gr = lag(va_agr_pc, n = 0, order_by = temporal) /
             lag(va_agr_pc, n = 1, order_by = temporal) - 1,
           va_ind_pc_gr = lag(va_ind_pc, n = 0, order_by = temporal) /
             lag(va_ind_pc, n = 1, order_by = temporal) - 1,
           va_agrind_pc_gr = lag(va_agrind_pc, n = 0, order_by = temporal) /
             lag(va_agrind_pc, n = 1, order_by = temporal) - 1,
           va_ser_pc_gr = lag(va_ser_pc, n = 0, order_by = temporal) /
             lag(va_ser_pc, n = 1, order_by = temporal) - 1,
           gdp_pc_gr = lag(gdp_pc, n = 0, order_by = temporal) /
             lag(gdp_pc, n = 1, order_by = temporal) - 1) %>%
    ungroup()

  # add recession dummy: 1 in case of a recession
  df$recession <- NA
  df[df$gdp_pc_gr > 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 0
  df[df$gdp_pc_gr <= 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 1

  # write data to disk
  saveRDS(df, "cache/df.rda")
} else {
  message(c("Reading previously saved prepared data to save time. Delete 'cache'
  directory to read and prepare data from scratch."))
  df <- readRDS("cache/df.rda")
}

# estimation ----
# relevel countries so that the US (or another country) are used as reference
# for the fixed effects estimation below
country_ref = "USA"
df <- mutate(df, spatial = relevel(spatial, ref = country_ref))

# split up data into separate dataframes for historic and scenario data
df_hist <- filter(df, scenario == "history")
df_scen <- filter(df, scenario != "history")

# for some countries (e.g. Aruba (ABW)) only historic population is available.
# need to filter out such cases as we need a list of countries for which
# estimation is actually possible to limit the scenario dataset, which is used
# for prediction, to these
df_hist <- filter(df_hist, !is.na(gdp), !is.na(pop))

# attach information about country area
country_area <- filter(df_hist, temporal == 2010) %>%
                  select(spatial, area) %>%
                  distinct() %>%
                  na.omit()

# drop old area column (only NAs)
df_scen <- select(df_scen, -area)

# attach country area information
df_scen <- inner_join(df_scen, country_area, by = c("spatial"))

# calculate population density
df_scen <- mutate(df_scen, pop_dens = pop/area)

# limit scenario data to countries that are present in the historic data
df_scen <- filter(df_scen, !(spatial %in% setequal(unique(df_scen$spatial),
                                                   unique(df_hist$spatial))))

model_agr <- lm(va_agr_pc ~ gdp_pc + recession, data = df_hist)
model_ind <- lm(va_ind_pc ~ gdp_pc + I(gdp_pc^2) + I(gdp_pc^3) + spatial +
                  recession + pop_dens, data = df_hist)

# prediction ----
# due to the fixed effect prediction can only be done for countries for which
# estimation has been carried out
countries_ind <- c(country_ref, as.character(unique(model_ind$model$spatial)))
countries_agr <- c(country_ref, as.character(unique(model_agr$model$spatial)))

# check if there are fixed effects present and predict accordingly
if(length(countries_agr) > 1){
  df_scen[df_scen$spatial %in% countries_agr, "va_agr_pc"] <-
  predict(model_agr, newdata = filter(df_scen, spatial %in% countries_agr))
} else {
  df_scen$va_agr_pc = predict(model_agr, newdata = df_scen)
}

# check if there are fixed effects present and predict accordingly
if(length(countries_ind) > 1){
  df_scen[df_scen$spatial %in% countries_ind, "va_ind_pc"] <-
  predict(model_ind, newdata = filter(df_scen, spatial %in% countries_ind))
} else {
  df_scen$va_ind_pc = predict(model_ind, newdata = df_scen)
}

# compute service sector as residual and level values
df_scen <- mutate(df_scen, va_ser_pc = gdp_pc - va_agr_pc - va_ind_pc,
                           va_agr = va_agr_pc * pop,
                           va_ind = va_ind_pc * pop,
                           va_ser = va_ser_pc * pop)
#
#
# result <- select(df_scen, -va_agr_pc_grf, -va_ind_pc_grf) %>%
#   rbind(df_hist) %>%
#   arrange(spatial)

result <- rbind(df_hist, df_scen)

result <- arrange(result, spatial)

# regional aggregation ----
map_region <- read.csv("sources/regions_definition.csv") %>%
  select(ISO, reg11) %>%
  filter(!(reg11 %in% c("INTship", "INTair", "glob"))) %>%
  rename(spatial = ISO)

result_reg <- inner_join(result, map_region, by = "spatial") %>%
  group_by(scenario, temporal, reg11) %>%
  summarise_each(funs(sum, "sum", sum(., na.rm = TRUE)), gdp, pop, va_ind, va_ser, va_agr) %>%
  ungroup() %>%
  rename(spatial = reg11)

names(result_reg) <- gsub("_sum", "", names(result_reg), fixed = TRUE)

# compute regional sector shares
result_reg <- mutate(result_reg, sum_va = va_agr + va_ind + va_ser,
                     share_agr = va_agr/sum_va,
                     share_ind = va_ind/sum_va,
                     share_ser = va_ser/sum_va)

# plotting ----
source("plotting.R")
