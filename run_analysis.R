library(reshape2)
library(ggplot2)
library(readr)
library(readxl)
library(countrycode)
library(dplyr)
library(openxlsx)
library(beepr)

rm(list = ls())

# disable scientific notation
options(scipen = 999)

# set warning level
options(warn = -1)

start_time <- Sys.time()

source("R/functions.R")

# load default settings
source("R/settings.R")

# prepare common data
source("R/prepare_data.R")

# default run ----
settings <- settings_default

# modify experiment name
settings$exp_name <- "levels"

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# experiment 1 ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "shares"

# use shares as explained variables
settings$lhs_levels <- FALSE

# # other modifications
settings$plotting <- TRUE

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# play a sound to that you know the run is finished and elasped time
beep(sound = 2, print(Sys.time() - start_time))
