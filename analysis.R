library(IDA)
library(reshape2)
library(ggplot2)

rm(list = ls())

# create output directories
if(!dir.exists("plots")) dir.create("plots", showWarnings = FALSE)

# prepare data ----
# vector of variables
# ATTENTION: when adding variables here, you have to make sure that the corresponding units are included in the filtering of 'idata' below
vars <- c("gdp" = "GDP",
          "va_agr" = "Value Added|Agriculture",
          "va_ind" = "Value Added|Industry",
          "va_man" = "Value Added|Manufacturing",
          "va_ser" = "Value Added|Services",
          "pop" = "Population",
          "area" = "Area")

# G20 memberstates (ATTENTION: other EU members not yet present, also: no
# sectoral data present for Canada)
g20 <- c("DEU", "ARG", "AUS", "BRA", "CHN", "FRA", "GBR", "IND", "IDN", "ITA", "JPN", "CAN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "USA")

wdi <- filter(idata, source_id == "WDI_2015",
              variable %in% vars, unit %in% c("bn USD2005/yr", "million", "km2"))

ssp_gdp <- filter(idata, source_id == "SSP", model == "OECD Env-Growth",
              variable == "GDP",
              unit == "bn USD2005/yr")

ssp_pop <- filter(idata, source_id == "SSP", model == "IIASA-WiC POP",
              variable == "Population",
              unit == "million")

df <- rbind(wdi, ssp_gdp, ssp_pop) # combine dataframe for renaming

rm(wdi, ssp_pop, ssp_gdp) # clean up

# rename variables
for (var in vars){
  df <- rename_var(df, var, names(vars)[vars == var])
}

# store units
units <- distinct(df, variable, unit)

df <- dcast(df, source_id + model + scenario + spatial + temporal ~ variable)

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
df <- group_by(df, source_id, model, scenario, spatial) %>%
  mutate(va_agr_pc_gr = lag(va_agr_pc, n = 0, order_by = temporal) / lag(va_agr_pc, n = 1, order_by = temporal) - 1,
         va_ind_pc_gr = lag(va_ind_pc, n = 0, order_by = temporal) / lag(va_ind_pc, n = 1, order_by = temporal) - 1,
         va_agrind_pc_gr = lag(va_agrind_pc, n = 0, order_by = temporal) / lag(va_agrind_pc, n = 1, order_by = temporal) - 1,
         va_ser_pc_gr = lag(va_ser_pc, n = 0, order_by = temporal) / lag(va_ser_pc, n = 1, order_by = temporal) - 1,
         gdp_pc_gr = lag(gdp_pc, n = 0, order_by = temporal) / lag(gdp_pc, n = 1, order_by = temporal) - 1) %>%
  ungroup()

# add recession dummy: 1 in case of a recession
df$recession <- NA
df[df$gdp_pc_gr > 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 0
df[df$gdp_pc_gr <= 0 & !(is.na(df$gdp_pc_gr)), "recession"] <- 1

# estimation ----
model_agr <- lm(va_agr_pc_gr ~ gdp_pc + recession, data = df)
model_ind <- lm(va_ind_pc_gr ~ gdp_pc + I(gdp_pc^2) + spatial +
                  recession + pop_dens, data = filter(df, spatial %in% g20))

# plotting ----
source("plotting.R")