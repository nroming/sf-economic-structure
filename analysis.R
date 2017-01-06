library(IDA)
library(reshape2)
library(ggplot2)

rm(list = ls())

start_time <- Sys.time()

# Settings -----
# Do not change the file 'R/settings.R' unless you really want to change
# the default settings and commit these changes. All short term changes to
# settings should be done in 'settings_actual.R'
if(!file.exists("settings.R")){
  message("No 'settings'-file found. Copying the default one from 'R/settings.R' to project root directory. This file is not under version control!")
  file.copy(from = "R/settings.R", to = "settings.R")
}

source("settings.R")

# create output directories
if(!dir.exists("figs")) dir.create("figs", showWarnings = FALSE)
if(!dir.exists("cache")) dir.create("cache", showWarnings = FALSE)

# prepare data ----
# moved to separate script
source("R/prepare_data.R")

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

model_agr <- lm(va_agr_pc ~ gdp_pc + spatial + recession + pop_dens, data = df_hist)
model_ind <- lm(va_ind_pc ~ gdp_pc + I(gdp_pc^2) + I(gdp_pc^3) + spatial +
                  recession + pop_dens, data = df_hist)
model_ser <- lm(va_ser_pc ~ gdp_pc + I(gdp_pc^2) + spatial +
                  recession + pop_dens, data = df_hist)

# prediction ----
# due to the fixed effect prediction can only be done for countries for which
# estimation has been carried out
countries_ind <- c(country_ref, as.character(unique(model_ind$model$spatial)))
countries_agr <- c(country_ref, as.character(unique(model_agr$model$spatial)))
countries_ser <- c(country_ref, as.character(unique(model_ser$model$spatial)))

# check if there are fixed effects present for agriculture and predict accordingly
if(length(countries_agr) > 1){
  df_scen[df_scen$spatial %in% countries_agr, "va_agr_pc"] <-
  predict(model_agr, newdata = filter(df_scen, spatial %in% countries_agr))
} else {
  df_scen$va_agr_pc = predict(model_agr, newdata = df_scen)
}

# check if there are fixed effects present for industry and predict accordingly
if(length(countries_ind) > 1){
  df_scen[df_scen$spatial %in% countries_ind, "va_ind_pc"] <-
  predict(model_ind, newdata = filter(df_scen, spatial %in% countries_ind))
} else {
  df_scen$va_ind_pc = predict(model_ind, newdata = df_scen)
}

# check if there are fixed effects present for services and predict accordingly
if(length(countries_ser) > 1){
  df_scen[df_scen$spatial %in% countries_ser, "va_ser_pc"] <-
  predict(model_ser, newdata = filter(df_scen, spatial %in% countries_ser))
} else {
  df_scen$va_ser_pc = predict(model_ser, newdata = df_scen)
}

# compute service sector as residual and level values
df_scen <- mutate(df_scen, #va_ser_pc = gdp_pc - va_agr_pc - va_ind_pc,
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
map_region <- read.csv("data/regions_definition.csv") %>%
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
if(plotting) source("R/plotting.R")

if(show_time) print(Sys.time() - start_time)
