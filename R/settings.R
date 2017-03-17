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