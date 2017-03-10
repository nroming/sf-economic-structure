library(reshape2)
library(ggplot2)
library(readr)
library(readxl)
library(countrycode)
library(dplyr)
library(openxlsx)

rm(list = ls())

start_time <- Sys.time()

source("R/functions.R")

# default run ----
exp_name = "default"
source("R/settings.R")

source("R/analysis.R")

# experiment 1 ----
exp_name = "experiment_626"
source("R/settings.R") # to reset settings to default
plotting <- FALSE

source("R/analysis.R")