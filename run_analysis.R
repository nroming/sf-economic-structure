rm(list = ls())

# disable scientific notation
options(scipen = 999)

# set warning level
options(warn = -1)

start_time <- Sys.time()

source("R/functions.R")

# define needed packages
packages <- c("reshape2", "ggplot2", "readr", "readxl", "countrycode", "dplyr", "openxlsx", "beepr")

# check if needed packags are installed and do so, if not
ipak(packages)

# load default settings
source("R/settings.R")

# prepare common data
source("R/prepare_data.R")

# reduced complexity run ----
settings <- settings_default

# modify experiment name
settings$exp_name <- "reduced"

# modify model complexity
settings$regressors <- c("gdp_pc", "I(gdp_pc^2)", "spatial", "recession",
                         "pop_dens", "temporal", "ratio_gdp_pc2glob")

settings$plotting <- TRUE

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# shares ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "full"

# # other modifications
settings$plotting <- TRUE

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# play a sound to that you know the run is finished and elasped time
beep(sound = 2, print(Sys.time() - start_time))
