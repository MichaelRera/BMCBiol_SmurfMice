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
library(knitr); library(kableExtra)

# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")
source("utils/seg.control.R")
source("utils/segmented.lme.R")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# C57_F ----------------------------------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load data -----------------------------------------------------------
data_c57_f <- readRDS(file = "data/clean/data_c57_f.rds")
var_names <- data_c57_f %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Run pipeline with ME  ------------------------------------------------
slm_tot_with_me <- lapply(var_names, run_pipeline, data_c57_f, use_me = TRUE,
                          file_path = "figure/final_whole_data_with_me/c57/female/c57_f_")

## Run pipeline without ME  ---------------------------------------------
slm_tot_without_me <- lapply(var_names, run_pipeline, data_c57_f, use_me = FALSE,
                             file_path = "figure/final_whole_data_without_me/c57/female/c57_f_")

## Define list of Breakpoints (BPs) ----
bps_with_me <- slm_tot_with_me %>% lapply(function(a) a$psi[, 2])
bps_with_me[sapply(bps_with_me, is.null)] = NA

bps_without_me <- slm_tot_without_me %>% lapply(function(a) a$psi[, 2])
bps_without_me[sapply(bps_without_me, is.null)] = NA

n_max_bp = max(bps_with_me %>% sapply(length), 
               bps_without_me %>% sapply(length))

## Summarize info in one table ---------
bps_compare_df = tibble("variable" = var_names, 
                        "with_me_bp1" = bps_with_me %>% sapply(function(a) a[1]) %>%  unlist,
                        "with_me_bp2" = bps_with_me %>% sapply(function(a) a[2]) %>% unlist,
                        "without_me_bp1" = bps_without_me %>% sapply(function(a) a[1]) %>% unlist,
                        "without_me_bp2" = bps_without_me %>% sapply(function(a) a[2]) %>% unlist)

## Add the result of the test for usage of Mixed effects:
use_me_test_output <- readRDS("results/me_used_c57_f_wo_bp_var.rds") %>% 
  setNames(c("variable", "use_me_test_result"))

bps_compare_df <- bps_compare_df %>% full_join(use_me_test_output)

## Show table
knitr::kable(bps_compare_df, format = "pipe")
## Save table
### As human-readable text
knitr::kable(bps_compare_df, format = "pipe") %>% 
  save_kable("results/use_me_test/compare_BP_c57_f_wrt_age.txt")
### As Excel-compatible file
bps_compare_df %>% write_tsv("results/use_me_test/compare_BP_c57_f_wrt_age.tsv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# C57_M ----------------------------------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load data -----------------------------------------------------------
data_c57_m <- readRDS(file = "data/clean/data_c57_m.rds")
var_names <- data_c57_m %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Run pipeline with ME  ------------------------------------------------
slm_tot_with_me <- lapply(var_names, run_pipeline, data_c57_m, use_me = TRUE,
                          file_path = "figure/final_whole_data_with_me/c57/male/c57_m_")

## Run pipeline without ME  ---------------------------------------------
slm_tot_without_me <- lapply(var_names, run_pipeline, data_c57_m, use_me = FALSE,
                             file_path = "figure/final_whole_data_without_me/c57/male/c57_m_")

## Define list of Breakpoints (BPs) ----
bps_with_me <- slm_tot_with_me %>% lapply(function(a) a$psi[, 2])
bps_with_me[sapply(bps_with_me, is.null)] = NA

bps_without_me <- slm_tot_without_me %>% lapply(function(a) a$psi[, 2])
bps_without_me[sapply(bps_without_me, is.null)] = NA

n_max_bp = max(bps_with_me %>% sapply(length), 
               bps_without_me %>% sapply(length))

## Summarize info in one table ---------
bps_compare_df = tibble(
  "variable" = var_names, 
  "with_me_bp1" = bps_with_me %>% sapply(function(a) a[1]) %>%  unlist,
  "with_me_bp2" = bps_with_me %>% sapply(function(a) a[2]) %>% unlist,
  "without_me_bp1" = bps_without_me %>% sapply(function(a) a[1]) %>% unlist,
  "without_me_bp2" = bps_without_me %>% sapply(function(a) a[2]) %>% unlist)

## Add the result of the test for usage of Mixed effects:
use_me_test_output <- readRDS("results/me_used_c57_m_wo_bp_var.rds") %>% 
  setNames(c("variable", "use_me_test_result"))

bps_compare_df <- bps_compare_df %>% full_join(use_me_test_output)

## Show table
knitr::kable(bps_compare_df, format = "pipe")
## Save table
### As human-readable text
knitr::kable(bps_compare_df, format = "pipe") %>% 
  save_kable("results/use_me_test/compare_BP_c57_m_wrt_age.txt")
### As Excel-compatible file
bps_compare_df %>% write_tsv("results/use_me_test/compare_BP_c57_m_wrt_age.tsv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AKRJ -----------------------------------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load data -----------------------------------------------------------
data_akrj_f <- readRDS(file = "data/clean/data_akrj_f.rds")
var_names <- data_akrj_f %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Run pipeline with ME  ------------------------------------------------
slm_tot_with_me <- lapply(var_names, run_pipeline, data_akrj_f, use_me = TRUE,
                          file_path = "figure/final_whole_data_with_me/c57/female/akrj_f_")

## Run pipeline without ME  ---------------------------------------------
slm_tot_without_me <- lapply(
  var_names, run_pipeline, data_akrj_f, use_me = FALSE,
  file_path = "figure/final_whole_data_without_me/c57/female/akrj_f_")

## Define list of Breakpoints (BPs) ----
bps_with_me <- slm_tot_with_me %>% lapply(function(a) a$psi[, 2])
bps_with_me[sapply(bps_with_me, is.null)] = NA

bps_without_me <- slm_tot_without_me %>% lapply(function(a) a$psi[, 2])
bps_without_me[sapply(bps_without_me, is.null)] = NA

n_max_bp = max(bps_with_me %>% sapply(length), 
               bps_without_me %>% sapply(length))

## Summarize info in one table ---------
bps_compare_df = tibble("variable" = var_names, 
                        "with_me_bp1" = bps_with_me %>% sapply(function(a) a[1]) %>%  unlist,
                        "with_me_bp2" = bps_with_me %>% sapply(function(a) a[2]) %>% unlist,
                        "without_me_bp1" = bps_without_me %>% sapply(function(a) a[1]) %>% unlist,
                        "without_me_bp2" = bps_without_me %>% sapply(function(a) a[2]) %>% unlist)

## Add the result of the test for usage of Mixed effects:
use_me_test_output <- readRDS("results/me_used_akrj_f_wo_bp_var.rds") %>% 
  setNames(c("variable", "use_me_test_result"))

bps_compare_df <- bps_compare_df %>% full_join(use_me_test_output)

## Show table
knitr::kable(bps_compare_df, format = "pipe")
## Save table
### As human-readable text
knitr::kable(bps_compare_df, format = "pipe") %>% 
  save_kable("results/use_me_test/compare_BP_akrj_f_wrt_age.txt")
### As Excel-compatible file
bps_compare_df %>% write_tsv("results/use_me_test/compare_BP_akrj_f_wrt_age.tsv")


