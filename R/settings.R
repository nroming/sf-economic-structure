settings_default <- list(

  # turn plotting on or off
  plotting = TRUE,

  #experiment name
  exp_name = "default",

  # reference country - used especially as the reference for fixed effects
  # estimation
  country_ref = "USA",

  # force a match between sum of sectoral value added and GDP
  force_sector_match_gdp = TRUE,

  # define right hand side of regression formula ----
  regressors = c("gdp_pc", "I(gdp_pc^2)", "I(gdp_pc^3)", "spatial", "recession",
                 "pop_dens", "temporal", "ratio_gdp_pc2glob")
)

# vector of variables
# ATTENTION: when adding variables here, you have to make sure that the
# corresponding units are included in the filtering of 'idata_n' in
# 'prepare_data.R'
vars <- c("gdp" = "GDP",
          "va_agr" = "Value Added|Agriculture",
          "va_ind" = "Value Added|Industry",
          "va_man" = "Value Added|Manufacturing",
          "va_ser" = "Value Added|Services",
          "pop" = "Population",
          "area" = "Area",
          "ca_share" = "Current account|GDP share",
          "ex" = "Exports|All",
          "im" = "Imports|All",
          "tax_share" = "Taxes|Share",
          "va_agr_share" = "Value Added|Agriculture|GDP share",
          "va_ind_share" = "Value Added|Industry|GDP share",
          "va_ser_share" = "Value Added|Services|GDP share")

# G20 memberstates (ATTENTION: other EU members not yet present, also: no
# sectoral data present for Canada)
g20 <- c("DEU", "ARG", "AUS", "BRA", "CHN", "FRA", "GBR", "IND", "IDN", "ITA",
         "JPN", "CAN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "USA")
