rm(list = ls())

# Library and load data --------------------------------------------------- 
library(magrittr) # use pipes: %>%
library(mgcv) # Spline regression
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
library(purrr) # for functional programming with data frames
library(gam) # for loess
library(caret) # for CV of loess span parameter
library(lubridate) # for handling dates
library(readr); library(readxl) # for intelligent reading of csv and xlsx 
library(stringr)

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# Load data ---------------------------------------------------------------
data_akrj_f <- readRDS(file = file.path("data/clean", data_dir, "data_akrj_f.rds")) %>% 
  filter(unnatural_death == FALSE)
# Load utility scripts ----------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

var_names <- data_akrj_f %>% 
  dplyr::select(-STATUS, -DBD, -mice, -AGE, -unnatural_death) %>% names()

## Were ME is used ---------------------------------------------------------
me_used <- sapply(var_names, is_me_necessary, data = data_akrj_f)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>%
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file = file.path("results", data_dir, "me_used_akrj_f.rds"))

# Estimate breakpoints -----------------------------------------------------
use_me_vect = readRDS(file = file.path("results", data_dir, "me_used_akrj_f.rds"))$me_used
use_me_vect = use_me_vect == "TRUE"

set.seed(098)
trycatch_wrapper <- function(y_var, use_me, ...) {
  return(tryCatch(run_pipeline(y_var, data = data_akrj_f, use_me, ...), error=function(e) NULL))
}
slm_tot <- mapply(
  trycatch_wrapper,
  var_names, use_me_vect,
  MoreArgs = list(file_path = file.path("figure", data_dir, "akrj/female/akrj_f_")))
slm_tot[sapply(slm_tot, length) == 0] = 'conv_error'

trycatch_wrapper(var_names[13], use_me_vect[13], data_akrj_f, 
                 file_path = file.path("figure", data_dir, "akrj/female/akrj_"))

# slm_tot <- mapply(function(x, y)  {
#                     run_pipeline(x, data_akrj_f, use_me = y,
#                   file_path = file.path("figure", data_dir, "akrj/female/akrj_f_"))
#                   }, var_names, use_me_vect)
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_akrj_f.rds"))

system(paste0("cd figure/", data_dir, "/akrj/female/ && pdftk akrj* cat output whole_akrj_f.pdf"))

## Move results from results in ME and NO_ME directories -------------------------------------
# slm_tot_me = readRDS(file = file.path("results", data_dir, "slm_tot_akrj_f_me.rds"))
# slm_tot_no_me = readRDS(file = file.path("results", data_dir, "slm_tot_akrj_f_no_me.rds"))
# slm_tot_me %>% sapply(length) %>% unname
# slm_tot_no_me %>% sapply(length) %>% unname
# slm_tot = slm_tot_no_me
# slm_tot[use_me_vect] = slm_tot_me[use_me_vect]
# 
# var_to_move = names(use_me_vect[use_me_vect])
# ## For all variables in var_to_move, move the file c_akrj_f_{var_to_move} from 
# ## figure/akrj/female_me to figure/akrj/female
# for (var in var_to_move) {
#   system(paste0("mv ~/work/side/two_phase_segmentation/figure/", data_dir,
#                 "/akrj/female_me/akrj_f_", var, ".pdf", 
#                 " ~/work/side/two_phase_segmentation/figure/", data_dir,
#                 "/akrj/female/akrj_f_", var, ".pdf"))
# }

## Number of breakpoints ---------------------------------------------------
number_of_bp <- slm_tot %>% sapply(function(a) a$selection.psi$npsi)
n_bp_df <- tibble(dataset = "akrj",
                       sexe = "female",
                       variable = var_names,
                       number_of_bp = number_of_bp)
saveRDS(n_bp_df, file = file.path("results", data_dir, "nb_of_bp_akrj_f.rds"))

# Estimate breakpoints with ME -------------------------------------------------

set.seed(098)
trycatch_wrapper <- function(y_var, ...) {
  return(tryCatch(run_pipeline(y_var, ...), error=function(e) NULL))
}

slm_tot <- lapply(var_names, trycatch_wrapper, data_akrj_f, use_me = TRUE,
                  file_path = file.path("figure", data_dir, "akrj/female_me/akrj_f_"))
slm_tot[sapply(slm_tot, length) == 0] = 'conv_error'
# slm_tot <- lapply(var_names, run_pipeline, data_akrj_f, use_me = TRUE,
#                   file_path = file.path("figure", data_dir, "akrj/female_me/akrj_f_"))
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_akrj_f_me.rds"))

system(paste0("cd figure/", data_dir, "/akrj/female_me/ && pdftk akrj* cat output whole_akrj_f.pdf"))

## Save breakpoints with ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_akrj_f_me.rds"))
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
bp_df <- tibble(dataset = "akrj_f_me",
                variable = var_names,
                number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_akrj_f_me.rds"))

# Estimate breakpoints without ME -------------------------------------------------
set.seed(098)
# problem with: FIDs

slm_tot <- lapply(var_names, trycatch_wrapper, data_akrj_f, use_me = FALSE,
                  file_path = file.path("figure", data_dir, "akrj/female_no_me/akrj_f_"))
# slm_tot <- lapply(var_names, run_pipeline, data_akrj_f, use_me = FALSE,
#                   file_path = file.path("figure", data_dir, "akrj/female_no_me/akrj_f_"))
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_akrj_f_no_me.rds"))

system(paste0("cd figure/", data_dir, "/akrj/female_no_me/ && pdftk akrj* cat output whole_akrj_f.pdf"))

## Save breakpoints with ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_akrj_f_no_me.rds"))
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
bp_df <- tibble(dataset = "akrj_f_no_me",
                variable = var_names,
                number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_akrj_f_no_me.rds"))

# Plotting data -----------------------------------------------------------
## Number of observations for each mice
data_akrj_f %>% group_by(mice) %>% tally() %>% `$`(n)
## Plotting all vars
pivoted <- data_akrj_f %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS),
               names_to = "var",
               values_to = "value")
ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
  xlab("Days before death") +
  facet_wrap(~var, scales = "free_y") +
  theme(legend.position = "none") + ggtitle("AKRJ Females")

# Run pipeline step by step --------------------------------------------------
names(data_akrj_f)
y_var <- "BT"
rawdata <- data_akrj_f %>%
  transmute(x = DBD,
            y = !!rlang::sym(y_var),
            Mice = as.factor(mice)) %>% 
  remove_missing()

## Zeroing out data ------------------------------------------------------
(p1 <- ggplot(rawdata, aes(x, y, color = Mice)) + geom_point() + geom_line() +
   theme(legend.position = "none"))
zo_data <- rawdata %>% 
  zero_out_lm() %>% 
  arrange(x)

## Plotting raw vs zeroed-out data
p2 <- ggplot(zo_data, aes(x, y, color = Mice)) + geom_point() + geom_line() +
  theme(legend.position = "none")
p1 + p2
## Visualize the zero_out effect
combined_df <-left_join(rawdata %>% dplyr::filter(Mice %in% zo_data$Mice), 
                        zo_data %>% transmute(x, y_mod = y, Mice)) %>% 
  dplyr::mutate(diff = y_mod - y)
ggplot(combined_df, aes(x = x, y = diff, color = Mice)) + 
  geom_point() + geom_line() + theme(legend.position = "none")

## Running segmented method ------------------------------------------------
lm1 <- lm(y ~ x + 1, data = zo_data)
slm1 <- selgmented(lm1, seg.Z = ~ x, type = "score", return.fit = TRUE)

## Plotting model  ------------------------------------------------
xseq <- c(min(zo_data$x), confint(slm1)[, 1], max(zo_data$x))
pred <- predict(slm1, data.frame(x = xseq))
xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200), 
           confint(slm1)[, 1]) %>% sort()

plot(zo_data$x, zo_data$y, col = "grey",
     xlab = "Remaining lifespan (days)",
     ylab = paste(y_var, "(modified for random effects) (mg/dl)"))
usr <- par("usr"); k <- 10; h <- (usr[4] - usr[3]) / k; y <- usr[3] + h
v <- (usr[2] - usr[1]) / k; x <- usr[1] + v * 3
lines(pred, x = xseq, col = "red", lwd = 2)
if (slm1$selection.psi$npsi > 0) {
  lines(slm1, term = "x", col = 2, k = 20, lwd = 2)
  for (ind in 1:nrow(confint(slm1))) {
    abline(v = confint(slm1)[ind], lty = 2, col = "lightblue", lwd = 2)
  }
}
## Textual elements
text(x, y + h * 1.5, paste("adjusted R^2 linear =",
                           signif(summary(lm1)$adj.r.squared, digits = 2)))
if (slm1$selection.psi$npsi > 0) { 
  text(x, y + h, paste("adjusted R^2 segmented =",
                       signif(summary(slm1)$adj.r.squared, digits = 2)))
  # text(x, y + h / 2, paste("p-value presence of BP:",
  #                          format.pval(slm1$selection.psi$pvalues[3], digits = 2)))
  text(x, y + h / 2, paste("p-value '0 vs 2 BP':",
                   format.pval(slm1$selection.psi$pvalues[1], digits = 2)))
  text(x, y, paste("p-value '1 vs 2 BP':",
                   format.pval(slm1$selection.psi$pvalues[2], digits = 3)))
  for (ind in 1:nrow(confint(slm1))) { # p-score doesn't work for multiple breakpoints
    text(confint(slm1)[ind], y, signif(confint(slm1)[ind], digits = 3))
  }
}
## CI
pred_slm1 <- predict(slm1, data.frame(x = xseq2), se.fit = TRUE)
crit_val <- qt(0.975, df = slm1$df.residual)
lines(pred_slm1$fit + crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
lines(pred_slm1$fit - crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
## Loess
# lines(predict(loess_cv(y ~ x, zo_data), data.frame(x = xseq2)), x = xseq2,
#       col = "blue", lwd = 1.4)
dev.copy2pdf(file = paste0("figure/", data_dir, "/akrj/female/akrj_f_", y_var, ".pdf"),
             width = 8, height = 6)
