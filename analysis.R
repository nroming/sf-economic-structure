library(IDA)
library(reshape2)
library(ggplot2)

rm(list = ls())

start_time <- Sys.time()

source("R/functions.R")

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
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
}

# prepare data ----
# moved to separate script
source("R/prepare_data.R")

# estimation ----
result_list <- prestimation(x = df,
                        spatial_ref = country_ref,
                        formula_agr = va_agr_pc ~ gdp_pc + spatial + recession + pop_dens,
                     formula_ind = va_ind_pc ~ gdp_pc + I(gdp_pc^2) + I(gdp_pc^3) + spatial + recession + pop_dens,
                     formula_ser = va_ser_pc ~ gdp_pc + I(gdp_pc^2) + spatial + recession + pop_dens)

result <- result_list$data

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
