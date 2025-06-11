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

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# Load data -----------------------------------------------------------
data_c57 = readRDS(file = file.path("data/clean", data_dir, "data_c57.rds")) %>% 
  filter(unnatural_death == FALSE)

# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

# Run whole pipeline  -----------------------------------------------------
var_names <- (data_c57) %>% dplyr::select(-STATUS, -DBD, -mice, -AGE, -sex, 
                                          -unnatural_death) %>% names()

## Get variables where only male or female has non NA values
var_only_one_sex = data_c57 %>%
  pivot_longer(cols = !c(mice, DBD, STATUS, sex, unnatural_death, AGE),
               names_to = "var", values_to = "value") %>%
  ## Remove NA values
  filter(!is.na(value)) %>%
  group_by(sex, var) %>%
  ## Count number of observations for each sex and variable
  tally() %>%
  ## Count number of variables
  group_by(var) %>% tally() %>% filter(n == 1) %>% pull(var)

## Remove them from var_names
var_names <- var_names[!var_names %in% var_only_one_sex]
# 
# 
# ###
# y_var = "BT"
# data <- data_c57
# use_me <- NULL
# file_path = "tmp/"
# control = seg.control(alpha = 10^-6)
# x_var = "DBD"; fe_bar = "sex"
# rawdata <- tibble(x = data[[x_var]],
#                   y = data[[y_var]],
#                   Mice = as.factor(data$mice),
#                   sex = data[[fe_var]]) %>%
#   remove_missing(na.rm = TRUE)
# fe_var = "sex"
# 
# is_me_necessary_sex("P5", data_c57)
# run_pipeline_sex("XYNm", data = data_c57, use_me = NULL)
# run_pipeline(var_names[[8]], data = data_c57, use_me = FALSE)
# ###
# 
# ## Plot XYNm with ggplot
# ggplot(data_c57, aes(DBD, EENm, color = as.factor(sex))) +
#   geom_point() + geom_line() +
#   theme(legend.position = "none")
# 
# y_var = "P5"; data = data_c57; use_me = NULL;
# file_path = "tmp/"; x_var = "DBD";
# control = seg.control(alpha = 10^-6);
# seed = FALSE; fe_var = "sex"

## Were ME is used ----------------------------------------------------------------
set.seed(6789)
me_used <- sapply(var_names, is_me_necessary_sex, data = data_c57, fe_var = "sex")
me_used_df <- me_used %>% sapply(function(a) ifelse(a == "NULL", "conv_error", a)) %>% 
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file.path("results", data_dir, "me_used_c57.rds"))

### Estimate breakpoints ----------------------------------------------------------
use_me_vect = readRDS(file.path("results", data_dir, "me_used_c57.rds"))$me_used
use_me_vect = use_me_vect == "TRUE"

set.seed(678)
trycatch_wrapper <- function(y_var, use_me, ...) {
  return(tryCatch(run_pipeline_sex(y_var, data = data_c57, use_me, ...), error=function(e) NULL))
}
slm_tot <- mapply(
  trycatch_wrapper,
  var_names, use_me_vect,
  MoreArgs = list(file_path = file.path("figure", data_dir, "c57/male_and_female/c57_")))
slm_tot[sapply(slm_tot, length) == 0] = 'conv_error'

trycatch_wrapper(var_names[34], use_me_vect[34])
run_pipeline_sex(var_names[33], use_me_vect[33])

# slm_tot <- mapply(function(x, y)  {
#   run_pipeline_sex(x, data_c57, use_me = y,
#                    file_path = file.path("figure", data_dir, "c57/male_and_female/c57_"))},
#   var_names, use_me_vect)
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57.rds"))

# run_pipeline_sex("XYDm", data = data_c57, use_me = use_me_vect["XYDm"])

system(paste0("cd figure/", data_dir, "/c57/male_and_female/ && pdftk c57* cat output whole_c57.pdf"))

## Number of breakpoints detected -------------------------------------------------
number_of_bp <- rep(NA, length(slm_tot))
number_of_bp[sapply(slm_tot, length) != 1] <- slm_tot[sapply(slm_tot, length) != 1] %>%
  sapply(function(a) a$selection.psi$npsi)
number_of_bp[sapply(slm_tot, length) == 1] = "conv_error"
n_bp_df <- tibble(dataset = "c57",
                  sex = "male",
                  variable = var_names,
                  number_of_bp = number_of_bp)
saveRDS(n_bp_df, file = file.path("results", data_dir, "nb_of_bp_c57.rds"))

### Estimate breakpoints with ME --------------------------------------------------
set.seed(678)
## Errors with var_names[32]:RERTOTm and var_names[41]:FAOX

slm_tot <- lapply(var_names, trycatch_wrapper, use_me = TRUE,
                  file_path = file.path("figure", data_dir, "c57/male_and_female_me/c57_"),
                  fe_var = "sex")
slm_tot[sapply(slm_tot, length) == 0] = 'conv_error'
names(slm_tot) <- var_names

# slm_tot <- lapply(var_names[-c(32, 41)], run_pipeline_sex, data_c57, use_me = TRUE,
#                   file_path = file.path("figure", data_dir, "c57/male_and_female_me/c57_"),
#                   fe_var = "sex")
# savee = slm_tot

slm_tot2 <- c(slm_tot[1:31], list("error"), 
              slm_tot[33:40], list("error"), slm_tot[42:length(var_names)])
names(slm_tot2) <- var_names

# ## Try run_pipeline_sex on RERTOTm
# (y_var = var_names[32])
# run_pipeline_sex(y_var, data = data_c57, use_me = TRUE, fe_var = "sex")
# 
# ## Try run_pipeline_sex on FAOX
# (y_var = var_names[41])
# run_pipeline_sex(y_var, data = data_c57, use_me = TRUE, fe_var = "sex")

saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57_me.rds"))
system(paste0("cd figure/", data_dir, "/c57/male_and_female_me/ && pdftk c57* cat output whole_c57.pdf"))

## Save breakpoints with ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_c57_me.rds"))
bp <- slm_tot %>% sapply(function(a) {
  if (length(a)!=1) {return(a$psi[, 2])}
  else {return("conv_error")}
})
bp = sapply(bp, 
            function(a) {
              if (length(a) == 0) {
                return("No BP")
              } else if (is.numeric(a)) {
                return(paste(round(a, 2), collapse = ", "))
              } else {
                return(a)
              }
            }
)
bp_df <- tibble(dataset = "c57_me",
                  variable = var_names,
                  number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_c57_me.rds"))

### Estimate breakpoints without ME ----------------------------------------------------------
set.seed(678)
slm_tot <- lapply(var_names, run_pipeline_sex, data_c57, use_me = FALSE,
                  file_path = file.path("figure", data_dir, "c57/male_and_female_no_me/c57_"),
                  fe_var = "sex")
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57_no_me.rds"))
system(paste0("cd figure/", data_dir, "/c57/male_and_female_no_me/ && pdftk c57* cat output whole_c57.pdf"))

## Save breakpoints without ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_c57_no_me.rds"))
bp <- slm_tot %>% sapply(function(a) {
  if (length(a)!=1) {return(a$psi[, 2])}
  else {return("conv_error")}
})
bp = sapply(bp, 
            function(a) {
              if (length(a) == 0) {
                return("No BP")
              } else if (is.numeric(a)) {
                return(paste(round(a, 2), collapse = ", "))
              } else {
                return(a)
              }
            }
)
bp_df <- tibble(dataset = "c57_no_me",
                variable = var_names,
                number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_c57_no_me.rds"))


# ## remove four mice: males, mice 36 37 38 39 for analysis by DBD and not by age
# # Plotting data -----------------------------------------------------------
# ## Number of observations for each mice
# data_c57 %>% group_by(mice) %>% tally() %>% `$`(n)
# ## Plotting all vars
# pivoted <- data_c57 %>% 
#   pivot_longer(cols = !c(mice, DBD, STATUS),
#                names_to = "var", values_to = "value")
# ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
#   geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
#   facet_wrap(~var, scales = "free_y") +
#   theme(legend.position = "none")
# 
# # Run pipeline step by step --------------------------------------------------
# names(data_c57)
# y_var <- "P0"
# rawdata <- data_c57 %>%
#   transmute(x = DBD %>% as.numeric(), y = !!rlang::sym(y_var),
#             Mice = as.factor(mice)) %>% 
#   remove_missing()
# 
# ## Zeroing out data ------------------------------------------------------
# p1 <- ggplot(rawdata, aes(x, y, color = Mice)) + geom_point() + geom_line() +
#   theme(legend.position = "none")
# zo_data <- rawdata %>% zero_out_lm() %>% arrange(x)
# 
# ## Plotting raw vs zeroed-out data
# p2 <- ggplot(zo_data, aes(x, y, color = Mice)) + geom_point() + geom_line() +
#   theme(legend.position = "none")
# p1 + p2
# ## Visualize the zero_out effect
# combined_df <-left_join(rawdata %>% dplyr::filter(Mice %in% zo_data$Mice), 
#                         zo_data %>% transmute(x, y_mod = y, Mice)) %>% 
#   dplyr::mutate(diff = y_mod - y)
# ggplot(combined_df, aes(x = x, y = diff, color = Mice)) + 
#   geom_point() + geom_line() + theme(legend.position = "none")
# 
# ## Running segmented method ------------------------------------------------
# lm1 <- lm(y ~ x + 1, data = zo_data)
# slm1 <- selgmented(lm1, seg.Z = ~ x, type = "score")
# 
# ## Plotting model  ------------------------------------------------
# xseq <- c(min(zo_data$x), confint(slm1)[, 1], max(zo_data$x))
# pred <- predict(slm1, data.frame(x = xseq))
# xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200), 
#            confint(slm1)[, 1]) %>% sort()
# 
# plot(zo_data$x, zo_data$y, col = "grey",
#      xlab = "Remaining lifespan (days)",
#      ylab = paste(y_var, "(modified for random effects) (mg/dl)"))
# usr <- par("usr"); k <- 10; h <- (usr[4] - usr[3]) / k; y <- usr[3] + h
# v <- (usr[2] - usr[1]) / k; x <- usr[1] + v * 3
# lines(pred, x = xseq, col = "red", lwd = 2)
# if (slm1$selection.psi$npsi > 0) {
#   lines(slm1, term = "x", col = 2, k = 20, lwd = 2)
#   for (ind in 1:nrow(confint(slm1))) {
#     abline(v = confint(slm1)[ind], lty = 2, col = "lightblue", lwd = 2)
#   }
# }
# ## Textual elements
# text(x, y + h * 1.5, paste("adjusted R^2 linear =",
#                            signif(summary(lm1)$adj.r.squared, digits = 2)))
# if (slm1$selection.psi$npsi > 0) { 
#   text(x, y + h, paste("adjusted R^2 segmented =",
#                        signif(summary(slm1)$adj.r.squared, digits = 2)))
#   text(x, y + h / 2, paste("p-value presence of BP:",
#                            format.pval(slm1$selection.psi$pvalues[3], digits = 2)))
#   # text(x, y, paste("p-value '0 vs 2 BP':",
#   #                  format.pval(slm1$selection.psi$pvalues[1], digits = 2)))
#   text(x, y, paste("p-value '1 vs 2 BP':",
#                    format.pval(slm1$selection.psi$pvalues[2], digits = 3)))
#   for (ind in 1:nrow(confint(slm1))) { # p-score doesn't work for multiple breakpoints
#     text(confint(slm1)[ind], y, signif(confint(slm1)[ind], digits = 3))
#   }
# }
# ## CI
# pred_slm1 <- predict(slm1, data.frame(x = xseq2), se.fit = TRUE)
# crit_val <- qt(0.975, df = slm1$df.residual)
# lines(pred_slm1$fit + crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
# lines(pred_slm1$fit - crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
# ## Loess
# # lines(predict(loess_cv(y ~ x, zo_data), data.frame(x = xseq2)), x = xseq2,
# #       col = "blue", lwd = 1.4)
# dev.copy2pdf(file = paste0("figure/", data_dir, "/c57/male_and_female/c57_m_", y_var, ".pdf"),
#              width = 8, height = 6)
# 
# 
