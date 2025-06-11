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
data_akrj_f <- readRDS(file = file.path("data/clean", data_dir, "data_akrj_f.rds"))
# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

# ## Were ME is used ---------------------------------------------------------
# me_used <- sapply(var_names, is_me_necessary, data = data_akrj_f)
# me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>%
#   tibble("variables" = names(.), "me_used" = .)
# saveRDS(me_used_df, file = file.path("results", data_dir, "me_used_akrj_f.rds"))


# Run whole pipeline  -----------------------------------------------------
var_names <- data_akrj_f %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()

# var_names <- var_names[!var_names %in% "P3"]
run_pipeline(var_names[[1]], data = data_akrj_f, use_me = FALSE, x_var = "AGE")

slm_tot <- lapply(var_names, run_pipeline, data_akrj_f, use_me = FALSE,
                  # file_path = file.path("figure", data_dir, "akrj_wrt_age/female/akrj_f_"),
                  file_path = "tmp/",
                  x_var = "AGE")


slm_tot <- lapply(var_names, run_pipeline, data_akrj_f, use_me = FALSE,
                  # file_path = file.path("figure", data_dir, "akrj_wrt_age/female/akrj_f_"),
                  file_path = "tmp/",
                  x_var = "AGE")
run_pipeline(var_names[[1]], data = data_akrj_f, use_me = FALSE, x_var = "AGE")
system(paste0("cd figure/", data_dir, "/akrj_wrt_age/female/ && pdftk akrj* cat output whole_akrj_f.pdf"))

## Plot pivoted data ----------------------------------------------
pivoted <- data_akrj_f %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, AGE),
               names_to = "var", values_to = "value")
ggplot(pivoted, aes(AGE, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") +
  theme(legend.position = "none")
plot(data_akrj_f$AGE, data_akrj_f$BW)
