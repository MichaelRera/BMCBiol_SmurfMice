rm(list = ls())

# Library and load data ---------------------------------------------------
library(magrittr) # use pipes: %>%
library(mgcv) # Spline regression
library(earth) # Spline regression with breakpoint detection
library(mda) # Does almost the same as the library "earth"
library(dplyr) # Manipulate data frames
library(openxlsx) # Open .xlsx
library(forcats)
library(tidyr) # for drop_na function
library(ggplot2)
library(PupillometryR) # for violin plots
library(patchwork) # for stacking ggplots
library(segmented)
library(dplyr)
library(gam) # for loess
library(caret) # for CV of loess span parameter
library(lubridate) # for handling dates
library(readr); library(readxl) # for intelligent reading of csv and xlsx 
library(stringr)

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# Load data -----------------------------------------------------------
#data_c57_f <- readRDS(file = file.path("data/clean", data_dir, "data_c57_f.rds"))

# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

var_names <- supFig5_glc_dbd %>% dplyr::select(-DBD, -AGE) %>% names()
test <- colnames(supFig5_glc_dbd)
test[9]<- "mice"
colnames(supFig5_glc_dbd)<- test
run_pipeline("Glucose", supFig5_glc_dbd, "DBD", use_me = FALSE)

