# This is the default settings file. Do not edit here.

# show how long it took the script to run?
show_time <- TRUE

# set warning level
options(warn = -1)

# turn plotting on or off
plotting <- TRUE

# disable scientific notation
options(scipen = 999)

# reference country - used especially as the reference for fixed effects
# estimation
country_ref = "USA"

# force a match between sum of sectoral value added and GDP
force_sector_match_gdp = TRUE

# create output directories ----
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
}

if(!dir.exists("output/data")){
  dir.create("output/data", recursive = TRUE, showWarnings = FALSE)
}

# define right hand side of regression formula ----
factors <- c("gdp_pc", "I(gdp_pc^2)", "I(gdp_pc^3)", "spatial", "recession", "pop_dens", "temporal", "ratio_gdp_pc2glob")

rhs <- list()

for (m in 1:length(factors)){
  tmp <-  combn(factors, m, simplify = FALSE)

  tmp <- lapply(tmp, paste0, collapse = " + ")

  rhs <- append(rhs, tmp)
}

rhs <- as.character(rhs)