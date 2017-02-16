library(reshape2)
library(ggplot2)
library(readr)
library(readxl)
library(countrycode)
library(dplyr)


rm(list = ls())

start_time <- Sys.time()

source("R/functions.R")

# Settings -----
# Do not change the file 'R/settings.R' unless you really want to change
# the default settings and commit these changes. All short term changes to
# settings should be done in 'settings.R' in project root directory
if(!file.exists("settings.R")){
  message("No 'settings'-file found. Copying the default one from 'R/settings.R' to project root directory. This file is not under version control!")
  file.copy(from = "R/settings.R", to = "settings.R")
}

source("settings.R")

# create output directories
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
}

if(!dir.exists("output/data")){
  dir.create("output/data", recursive = TRUE, showWarnings = FALSE)
}

# prepare data ----
# moved to separate script
source("R/prepare_data.R")

# estimation ----
# define right hand side of regression formula
rhs <- c("gdp_pc + spatial + recession",
         "gdp_pc + spatial + recession + pop_dens",
         "gdp_pc + spatial + recession + pop_dens + gdp",
         "gdp_pc + spatial + recession + pop_dens + pop",
         "gdp_pc + spatial + recession + pop_dens + gdp + pop",
         "log(gdp_pc) + spatial + recession",
         "log(gdp_pc) + spatial + recession + log(gdp)",
         "log(gdp_pc) + spatial + recession + log(pop)",
         "gdp_pc + I(gdp_pc^2) + spatial + recession + pop_dens",
         "gdp_pc + I(gdp_pc^2) + I(gdp_pc^3) + spatial + recession + pop_dens")

# determine formulas
formula_agr <- paste("va_agr_pc", "~", rhs)
formula_ind <- paste("va_ind_pc", "~", rhs)
formula_ser <- paste("va_ser_pc", "~", rhs)

# preallocate lsit for results
models_agr <- list()
models_ind <- list()
models_ser <- list()

# preallocate dataframes for adjusted R-squared
rsq_agr <- data.frame()
rsq_ind <- data.frame()
rsq_ser <- data.frame()

# model selection ----
for (i in 1:length(rhs)){
  models_agr[[i]] <- lm(as.formula(formula_agr[i]), data = df)
  models_ind[[i]] <- lm(as.formula(formula_ind[i]), data = df)
  models_ser[[i]] <- lm(as.formula(formula_ser[i]), data = df)

  rsq_agr[i, "formula"] <- formula_agr[i]
  rsq_agr[i, "r_sq_adj"] <- summary(models_agr[[i]])$adj.r.squared

  rsq_ind[i, "formula"] <- formula_ind[i]
  rsq_ind[i, "r_sq_adj"] <- summary(models_ind[[i]])$adj.r.squared

  rsq_ser[i, "formula"] <- formula_ser[i]
  rsq_ser[i, "r_sq_adj"] <- summary(models_ser[[i]])$adj.r.squared
}

# select best model for each sector
best_agr <- filter(rsq_agr, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()
best_ind <- filter(rsq_ind, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()
best_ser <- filter(rsq_ser, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()

# estimation ---
result_list <- prestimation(x = df,
                            spatial_ref = country_ref,
                            formula_agr = best_agr,
                            formula_ind = best_ind,
                            formula_ser = best_ser)

result <- result_list$data

# regional aggregation ----
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
