rm(list = ls())

# Library and load data ---------------------------------------------------
library(magrittr) # use pipes: %>%
library(mgcv) # Spline regression
library(mda) # Does almost the same as the library "earth"
library(dplyr) # Manipulate data frames
library(openxlsx) # Open .xlsx
library(forcats) # for handling factors
library(tidyr) # for drop_na function
library(ggplot2) # plots
library(PupillometryR) # for violin plots
library(patchwork) # for stacking ggplots
library(purrr) # for functional programming with data frames
library(gam) # for loess
library(caret) # for CV of loess span parameter
library(lubridate) # for handling dates
library(readr); library(readxl) # for intelligent reading of csv and xlsx 
library(stringr) # for handling strings
library(segmented)

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# Load data -----------------------------------------------------------
data_c57_m <- readRDS(file = file.path("data/clean", data_dir, "data_c57_m.rds"))
# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

# Run whole pipeline  -----------------------------------------------------
var_names <- data_c57_m %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()

# var_names <- var_names[!var_names %in% "P3"]
run_pipeline(var_names[[1]], data = data_c57_m, use_me = FALSE, x_var = "AGE")
###
y_var = "BW"
data <- data_c57_m
use_me <- FALSE
file_path = "tmp/"
control = seg.control(alpha = 10^-6)
x_var = "AGE"
###
run_pipeline(y_var, data = data_c57_m, use_me = FALSE, x_var = "AGE")

slm_tot <- lapply(var_names, run_pipeline, data_c57_m, use_me = FALSE,
                  file_path = file.path("figure", data_dir, "c57_wrt_age/male/c57_m_"),
                  x_var = "AGE")
system(paste0("cd figure/", data_dir, "/c57_wrt_age/male/ && pdftk c57* cat output whole_c57_m.pdf"))

## Plot pivoted data ----------------------------------------------
pivoted <- data_c57_m %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, AGE),
               names_to = "var", values_to = "value")
ggplot(pivoted, aes(AGE, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") +
  theme(legend.position = "none")
plot(data_c57_m$AGE, data_c57_m$GLY)
