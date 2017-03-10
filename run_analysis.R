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