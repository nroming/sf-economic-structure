rm(list = ls())

# disable scientific notation
options(scipen = 999)

# set warning level
options(warn = -1)

start_time <- Sys.time()

source("R/functions.R")

# define needed packages
packages <- c("reshape2", "ggplot2", "readr", "readxl", "countrycode", "dplyr", "openxlsx", "beepr", "zoo", "foreach", "doParallel")

# check if needed packags are installed and do so, if not
ipak(packages)

# load default settings
source("R/settings.R")

# prepare common data
source("R/prepare_data.R")

# population weights ----
settings <- settings_default

# modify experiment name
settings$exp_name <- "weights_pop"

# modify model complexity
settings$regressors <- c("gdp_pc", "I(gdp_pc^2)", "spatial", "recession",
                         "pop_dens", "urb_share", "temporal", "ratio_gdp_pc2glob")

# use weighed regression
settings$regression_weights <- "pop"

settings$plotting <- FALSE

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# population weights with nx ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "weights_pop_nx"

# modify model complexity
settings$regressors <- c("gdp_pc", "I(gdp_pc^2)", "spatial", "recession",
                         "pop_dens", "urb_share", "nx_pc_share", "temporal", "ratio_gdp_pc2glob")

# # other modifications
settings$plotting <- FALSE

# use weighed regression
settings$regression_weights <- "pop"

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# gdp weights ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "weights_gdp"

# modify model complexity
settings$regressors <- c("gdp_pc", "I(gdp_pc^2)", "spatial", "recession",
                         "pop_dens", "urb_share", "temporal", "ratio_gdp_pc2glob")

# # other modifications
settings$plotting <- FALSE

# use weighed regression
settings$regression_weights <- "gdp"

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# gdp weights with nx ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "weights_gdp_nx"

# modify model complexity
settings$regressors <- c("gdp_pc", "I(gdp_pc^2)", "spatial", "recession",
                         "pop_dens", "urb_share", "nx_pc_share", "temporal", "ratio_gdp_pc2glob")

# # other modifications
settings$plotting <- FALSE

# use weighed regression
settings$regression_weights <- "gdp"

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# compare runs ---
source("R/compare_runs.R")

# play a sound to that you know the run is finished and elasped time
beep(sound = 2, print(Sys.time() - start_time))
