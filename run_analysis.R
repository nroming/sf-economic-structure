library(reshape2)
library(ggplot2)
library(readr)
library(readxl)
library(countrycode)
library(dplyr)
library(openxlsx)

rm(list = ls())

# disable scientific notation
options(scipen = 999)

# set warning level
options(warn = -1)

start_time <- Sys.time()

source("R/functions.R")

# load default settings
source("R/settings.R")

# default run ----
settings <- settings_default

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# experiment 1 ----
# load default settings
settings <- settings_default

# modify experiment name
settings$exp_name <- "experiment_626"

# other modifications
settings$plotting <- FALSE

# prepare run
settings <- prepare_run(settings)

source("R/analysis.R")

# display elapsed time
print(Sys.time() - start_time)