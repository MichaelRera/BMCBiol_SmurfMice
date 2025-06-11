rm(list = ls())
# Library and load data ---------------------------------------------------
library(magrittr) # use pipes: %>%
library(mgcv) # Spline regression
library(earth) # Spline regression with breakpoint detection
library(mda) # Does almost the same as the library "earth"
library(dplyr) # Manipulate data frames
library(openxlsx) # Open .xlsx
library(forcats) # for handling factors
library(tidyr) # for drop_na function
library(ggplot2)
library(PupillometryR) # for violin plots
library(patchwork) # for stacking ggplots
library(segmented)
library(dplyr)
library(purrr) # for functional programming with data frames
library(gam) # for loess
library(caret) # for CV of loess span parameter
library(lubridate) # for handling dates
library(readr); library(readxl) # for intelligent reading of csv and xlsx 
library(stringr)
library(clipr)
library(knitr)

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")


# Modified is_me_necessary function ---------------------------------------
is_me_necessary <- function(y_var, data) {
  ## Standardize dataset
  data_tmp <- data %>%
    transmute(x = DBD,
              y = !!rlang::sym(y_var),
              Mice = as.factor(mice)) %>%
    remove_missing(na.rm = TRUE)
  ## Remove mice that have too few observations to perform computation: 3 or less data points
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>%
    filter(n <= 3) %>%
    '$'(Mice)
  data <- data_tmp %>%
    filter(!(Mice %in% mice_with_too_few_obs)) %>%
    mutate(Mice = droplevels(Mice))
  ## Run linear model with mixed effect of Mice onto the slope and intercept of the y variable
  lme1 = lme(y ~ x,
              random = list(Mice = pdDiag(~ 1 + x)), 
              data = data)
  ## Run segmented linear model with similar mixed effects.
  ## Since segmented.lme is prone to computational errors, we use tryCatch and
  ## set use_me to FALSE if an error was returned
  slme1 <- tryCatch(
    segmented.lme(lme1, ~x, random = list(Mice = pdDiag(~ 1 + x)), data = data),
    error = function(e) {
      warning("Warning: is_me_necessary ran into an error. use_me set to FALSE.");
      NULL
    })
  if (is.null(slme1)) {
    use_me <- NULL
  } else {
    assign("data_global", data, envir = .GlobalEnv)
    lm1 <- lm(y ~ x, data = data_global)
    slm1 <- segmented(lm1, seg.Z = ~ x)
    rm(data_global, envir = global_env()) # rm the variable created in the global env.
    lower.psi_slme1 <- confint(slme1)[1, 4]
    upper.psi_slme1 <- confint(slme1)[2, 4]
    ci_with_me <- upper.psi_slme1 - lower.psi_slme1
    ci_without_me <- confint(slm1)[3] - confint(slm1)[2]
    ci_prop_thresh <- 0.75 # use a threshold of 0.75
    # If CI with mixed effect is smaller than 0.75 * CI without me,
    # it is estimated that it is relevant to use mixed effect
    use_me <- ci_with_me / ci_without_me < ci_prop_thresh
  }
  return(use_me)
}

# c57_m -----------------------------------------------------------
data_c57_m <- readRDS(file = file.path("data/clean", data_dir, "data_c57_m.rds"))

var_names <- (data_c57_m) %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Were ME was used -----
me_used <- sapply(var_names, is_me_necessary, data = data_c57_m)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>% 
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file.path("results", data_dir, "me_used_c57_m_wo_bp_var.rds"))

# c57_f -----------------------------------------------------------
data_c57_f <- readRDS(file = file.path("data/clean", data_dir, "data_c57_f.rds"))

var_names <- (data_c57_f) %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Were ME was used -----
me_used <- sapply(var_names, is_me_necessary, data = data_c57_f)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>% 
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file.path("results", data_dir, "me_used_c57_f_wo_bp_var.rds"))

# c57 -----------------------------------------------------------
data_c57 <- readRDS(file = file.path("data/clean", data_dir, "data_c57.rds"))

var_names <- (data_c57) %>% dplyr::select(-STATUS, -DBD, -mice, -AGE, -sex) %>% names()
## Were ME was used -----
me_used <- sapply(var_names, is_me_necessary_fe, data = data_c57)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>% 
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file.path("results", data_dir, "me_used_c57_wo_bp_var.rds"))

# akrj_f -----------------------------------------------------------
data_akrj_f <- readRDS(file = file.path("data/clean", data_dir, "data_akrj_f.rds"))

var_names <- (data_akrj_f) %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()
## Were ME was used -----
me_used <- sapply(var_names, is_me_necessary, data = data_akrj_f)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>%
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file.path("results", data_dir, "me_used_akrj_f_wo_bp_var.rds"))


# Collate results ---------------------------------------------------------
c57_m <- readRDS(file.path("results", data_dir, "me_used_c57_m_wo_bp_var.rds")) %>% setNames(c("variables", "me_used_c57_m"))
c57_f <- readRDS(file.path("results", data_dir, "me_used_c57_f_wo_bp_var.rds")) %>% setNames(c("variables", "me_used_c57_f"))
akrj <-  readRDS(file.path("results", data_dir, "me_used_akrj_f_wo_bp_var.rds")) %>% setNames(c("variables", "me_used_akrj"))

me_used_df <- akrj %>% full_join(c57_m) %>% full_join(c57_f)
kable(me_used_df, format = "pipe")
write_last_clip()

## With C57 male and female together
c_57_m <- readRDS(file.path("results", data_dir, "me_used_c57_m.rds")) %>% setNames(c("variable", "me_used_c57_m"))
c_57_f <- readRDS(file.path("results", data_dir, "me_used_c57_f.rds")) %>% setNames(c("variable", "me_used_c57_f"))
c_57 <- readRDS(file.path("results", data_dir, "me_used_c57.rds")) %>% setNames(c("variable", "me_used_c57"))
akrj <-  readRDS(file.path("results", data_dir, "me_used_akrj_f.rds")) %>% setNames(c("variable", "me_used_akrj_f"))
me_used_df <- akrj %>% full_join(c_57) %>% full_join(c_57_m) %>% full_join(c_57_f)

## BP locations
c_57_m_no_me <- readRDS(file.path("results", data_dir, "bp_c57_m_no_me.rds"))
c_57_f_no_me <- readRDS(file.path("results", data_dir, "bp_c57_f_no_me.rds"))
c_57_no_me <- readRDS(file.path("results", data_dir, "bp_c57_no_me.rds"))
akrj_f_no_me <- readRDS(file.path("results", data_dir, "bp_akrj_f_no_me.rds"))
c_57_m_me <- readRDS(file.path("results", data_dir, "bp_c57_m_me.rds"))
c_57_f_me <- readRDS(file.path("results", data_dir, "bp_c57_f_me.rds"))
c_57_me <- readRDS(file.path("results", data_dir, "bp_c57_me.rds"))
akrj_f_me <- readRDS(file.path("results", data_dir, "bp_akrj_f_me.rds"))
# bind_rows all dafaframes
bp_df = bind_rows(akrj_f_no_me, akrj_f_me, c_57_no_me, c_57_me, c_57_m_no_me, c_57_m_me, c_57_f_no_me, c_57_f_me)
## pivot_wider: var_name = dataset and var_value = number_of_bp
bp_df <- bp_df %>% pivot_wider(names_from = dataset, values_from = number_of_bp)
# Sort by alphabetical order of variable 
bp_df <- bp_df[order(bp_df$variable), ]

me_used_and_bp = me_used_df %>% full_join(bp_df)

## Subset only variables whose name contain the string "_me"
variables_that_match = me_used_and_bp %>% names %>% str_detect("_me")
subset = me_used_and_bp[variables_that_match]
## Which columns contain the element "conv_error"
conv_error = subset %>% sapply(function(a) "conv_error" %in% a)

## for var_name in which(conv_error):
for (var_name in which(conv_error)) {
  ## Get indices where "conv_error" is present
  conv_error_indices = which(subset[[var_name]] == "conv_error")
  ## Get values of use_me_var_name where "conv_error" is present
  me_used_and_bp[[var_name]][conv_error_indices]
}

## For akrj_f_me:
## Get indices where "conv_error" is present
akrj_f_me_conv_error = which(subset$akrj_f_me == "conv_error")
## Get values of use_me_akrj_f_me where "conv_error" is present
me_used_and_bp$me_used_akrj_f[akrj_f_me_conv_error]

## Same for c57_me:
c57_me_conv_error = which(subset$c57_me == "conv_error")
me_used_and_bp$me_used_c57[c57_me_conv_error]

## Same for c57_f_me:
c57_f_me_conv_error = which(subset$c57_f_me == "conv_error")
me_used_and_bp$me_used_c57_f[c57_f_me_conv_error]

## Sort by alphabetical order of the variable "variable"
me_used_and_bp <- me_used_and_bp[order(me_used_and_bp$variable), ]

## Save to csv
write.csv(me_used_and_bp, file = file.path("results", data_dir, "me_used_and_bp.csv"), row.names = FALSE)
knitr::kable(me_used_and_bp, format = "pipe")
write_last_clip()

## C57 results
df = read.csv(file.path("results", data_dir, "me_used_and_bp.csv"))
c57_bp = ifelse(df$me_used_c57 == "TRUE", df$c57_me, df$c57_no_me)
names(c57_bp) = df$variable
aaa = data.frame(names(c57_bp), c57_bp)
rownames(aaa) = NULL

## C57_M results
df = read.csv(file.path("results", data_dir, "me_used_and_bp.csv"))
c57_m_bp = ifelse(df$me_used_c57_m == "TRUE", df$c57_m_me, df$c57_m_no_me)
names(c57_m_bp) = df$variable
aaa_m = data.frame(names(c57_m_bp), c57_m_bp)
rownames(aaa_m) = NULL
aaa_m
df %>% dplyr::select(variable, me_used_c57_m, c57_m_me, c57_m_no_me)

## C57_F results
df = read.csv(file.path("results", data_dir, "me_used_and_bp.csv"))
c57_f_bp = ifelse(df$me_used_c57_f == "TRUE", df$c57_f_me, df$c57_f_no_me)
names(c57_f_bp) = df$variable
aaa_f = data.frame(names(c57_f_bp), c57_f_bp)
rownames(aaa_f) = NULL
aaa_f
df %>% dplyr::select(variable, me_used_c57_f, c57_f_me, c57_f_no_me)





