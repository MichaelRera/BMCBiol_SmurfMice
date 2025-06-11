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
data_c57_f <- readRDS(file = file.path("data/clean", data_dir, "data_c57_f.rds"))

# Load utils scripts --------------------------------------------------
source("utils/utils_pipeline_function.R")
source("utils/zeroing_out_functions.R")

var_names <- data_c57_f %>% dplyr::select(-STATUS, -DBD, -mice, -AGE) %>% names()

## Were ME is used ---------------------------------------------------------
me_used <- sapply(var_names, is_me_necessary, data = data_c57_f)
me_used_df <- me_used %>% sapply(function(a) ifelse(is.null(a), "conv_error", a)) %>% 
  tibble("variables" = names(.), "me_used" = .)
saveRDS(me_used_df, file = file.path("results", data_dir, "me_used_c57_f.rds"))

# Estimate breakpoints  -----------------------------------------------------
use_me_vect = readRDS(file = file.path("results", data_dir, "me_used_c57_f.rds"))$me_used
use_me_vect = use_me_vect == "TRUE"

slm_tot <- mapply(function(x, y)  {
  run_pipeline(x, data_c57_f, use_me = y,
               file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
}, var_names, use_me_vect)

saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57_f.rds"))
system(paste0("cd figure/", data_dir, "/c57/female/ && pdftk c57* cat output whole_c57_f.pdf"))

## Move results from results in ME and NO_ME directories -------------------------------------
# slm_tot_me = readRDS(file = file.path("results", data_dir, "slm_tot_c57_f_me.rds"))
# slm_tot_no_me = readRDS(file = file.path("results", data_dir, "slm_tot_c57_f_no_me.rds"))
# slm_tot = slm_tot_no_me
# slm_tot[use_me_vect] = slm_tot_me[use_me_vect]

# var_to_move = names(use_me_vect[use_me_vect])
# ## For all variables in var_to_move, move the file c_57_f_{var_to_move} from 
# ## figure/c57/female_me to figure/c57/female
# for (var in var_to_move) {
#   system(paste0("mv ~/work/side/two_phase_segmentation/figure/", data_dir,
#                 "/c57/female_me/c57_f_", var, ".pdf", 
#                 " ~/work/side/two_phase_segmentation/figure/", data_dir,
#                 "/c57/female/c57_f_", var, ".pdf"))
# }

## Number of breakpoints --------------------------------------------------
number_of_bp <- slm_tot %>% sapply(function(a) a$selection.psi$npsi)
n_bp_df <- tibble(dataset = "c57",
                  sex = "female",
                  variable = var_names,
                  number_of_bp = number_of_bp)
saveRDS(n_bp_df, file = file.path("results", data_dir, "/nb_of_bp_c57_f.rds"))

# Estimate breakpoints with ME -----------------------------------------------------

# ## Problematic variables:
# var_names[22]
# var_names[8]
# var_names[39]
# slm_tot1 <- lapply(var_names[1:7], run_pipeline, data_c57_f, use_me = TRUE,
#                    file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
# slm_tot2 <- lapply(var_names[9:21], run_pipeline, data_c57_f, use_me = TRUE,
#                    file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
# slm_tot3 <- lapply(var_names[23:38], run_pipeline, data_c57_f, use_me = TRUE,
#                    file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
# slm_tot4 <- lapply(var_names[40:length(var_names)], run_pipeline, data_c57_f, use_me = TRUE,
#                    file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
# 
# slm_tot = c(slm_tot1, list("conv_error"), slm_tot2, list("conv_error"), slm_tot3, 
#             list("conv_error"), slm_tot4)

set.seed(678)
trycatch_wrapper <- function(y_var, ...) {
  return(tryCatch(run_pipeline(y_var, ...), error=function(e) NULL))
}

slm_tot <- lapply(var_names, trycatch_wrapper, data_c57_f, use_me = TRUE,
                  file_path = file.path("figure", data_dir, "c57/female_me/c57_f_"))
slm_tot[sapply(slm_tot, length) == 0] = 'conv_error'

names(slm_tot) = var_names
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57_f_me.rds"))
system(paste0("cd figure/", data_dir, "/c57/female_me/ && pdftk c57* cat output whole_c57_f.pdf"))

## Save breakpoints with ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_c57_f_me.rds"))
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

bp_df <- tibble(dataset = "c57_f_me",
                variable = var_names,
                number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_c57_f_me.rds"))

# Estimate breakpoints without ME -----------------------------------------------------
slm_tot <- lapply(var_names, run_pipeline, data_c57_f, use_me = FALSE,
                  file_path = file.path("figure", data_dir, "c57/female_no_me/c57_f_"))
saveRDS(slm_tot, file = file.path("results", data_dir, "slm_tot_c57_f_no_me.rds"))
system(paste0("cd figure/", data_dir, "/c57/female_no_me/ && pdftk c57* cat output whole_c57_f.pdf"))

## Save breakpoints without ME --------------------------------------------------------------
slm_tot = readRDS(file = file.path("results", data_dir, "slm_tot_c57_f_no_me.rds"))
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
bp_df <- tibble(dataset = "c57_f_no_me",
                variable = var_names,
                number_of_bp = bp)
saveRDS(bp_df, file = file.path("results", data_dir, "bp_c57_f_no_me.rds"))

# Plotting data -----------------------------------------------------------
## Number of observations for each mice
data_c57_f %>% group_by(mice) %>% tally() %>% `$`(n)
## Plotting GLY var -----------------------------------------------------
ggplot(data_c57_f, aes(DBD, GLY, color = as.factor(mice))) +
  geom_point() + geom_line() + scale_x_time() +
  theme(legend.position = "none")
## Plotting all vars -----------------------------------------------------
pivoted <- data_c57_f %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS),
               names_to = "var",
               values_to = "value")
ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") + scale_x_time() +
  theme(legend.position = "none")

# Run pipeline step by step --------------------------------------------------
names(data_c57_f)
# Done: BW, LEAN, FAT, GLY, BT, P0 (makes the segmented.lme method crash in zo),
# P1 (0 BP, graph to be edited), P3 (makes the segmented.lme method crash in zo),
# P5 (runs fine but results are up instead of down), VO2Dm, VCO2Dm, VCO2Nm,
# RERDm (potential problem in the zo, the slope is not accounted for; + outlier),
# RERNm (slope is not accounted for), EEDm, EENm, FINs, WIDs, WINs, ACTDm,
# ACTNm, ACTZDs, ACTZNs, DOB, DOD, AGE, ACTTOTs, ACTZTOTs, RERTOTm, EETOTs, FITOTKCALs, WITOTs, FATPROP, LEANPROP, EB, FAOX, DBD

y_var <- "ACTZDs"
rawdata <- data_c57_f %>% 
  transmute(x = DBD,
            y = !!rlang::sym(y_var),
            Mice = as.factor(mice)) %>% 
  remove_missing()

## Zeroing out data ------------------------------------------------------
set.seed(09876) # For reproducibility because there is randomness in the 
# function zero_out_slope (bootstrapping)
# set.seed(098467)
(p1 <- ggplot(rawdata, aes(x, y, color = Mice)) + geom_point() + geom_line() +
    theme(legend.position = "none"))
zo_data <- rawdata %>% 
  zero_out_slope(psi_init = -20) %>% 
  arrange(x)
## Note: using psi_init = quantile(rawdata$x, 0.9, na.rm = TRUE) instead of 
## the median works better for cases like VCO2Dm where there is a BP zero close
## to zero

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
slm1 <- selgmented(lm1, seg.Z = ~ x, type = "score")
## TODO: see what the p.values correspond to and how to retrieve them

## For running Davies test
slm1$call <- segmented(lm1, seg.Z = ~ x)$call
# davies.test(slm1_sel)

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
if (!is.null(slm1$psi)) {
  lines(slm1, term = "x", col = 2, k = 20, lwd = 2)
  for (ind in 1:nrow(confint(slm1))) {
    abline(v = confint(slm1)[ind], lty = 2, col = "lightblue", lwd = 2)
  }
}
## Textual elements
text(x, y + h * 1.5, paste("adjusted R^2 linear =",
                           signif(summary(lm1)$adj.r.squared, digits = 2)))
if (!is.null(slm1$psi)) { 
  text(x, y + h, paste("adjusted R^2 segmented =",
                       signif(summary(slm1)$adj.r.squared, digits = 2)))
  text(x, y + h / 2, paste("p-value absence of 1 BP:",
                           signif(davies.test(slm1)$p.value, digits = 3)))
  text(x, y, paste("p-value absence of a 2nd BP:",
                   signif(pscore.test(slm1)$p.value, digits = 3)))
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
dev.copy2pdf(file = paste0("figure/", data_dir, "/c57/female/c57_f_", y_var, ".pdf"),
             width = 8, height = 6)


# For further analysis --- legacy -----------------------------------------
## Study of difference between zo_slope and zo_slope_diff
## Code to be used in script `pipeline.R`
# ggplot(rawdata, aes(x, y, color = Mice)) + geom_point() + geom_line() +
#   theme(legend.position = "none") + facet_wrap(~Mice)
# ggplot(zo3_data, aes(x, y, color = Mice)) + geom_point() + geom_line() +
#   theme(legend.position = "none") + facet_wrap(~Mice)
# zo_data <- rawdata %>% 
#   zero_out_slope_diff("x", "y", psi_init = median(rawdata$x, na.rm = TRUE)) %>% 
#   arrange(x) %>% 
#   mutate(y = y_mod)
# all_data <- rbind(rawdata %>% mutate(type = "raw") %>% dplyr::filter(Mice %in% zo_data$Mice),
#                   zo_data %>% mutate(type = "zo_slope") %>% dplyr::select(-y_mod))#,
#                   # zo3_data %>% mutate(type = "zo_slope_no_seg") %>% dplyr::select(-slope, -intercept))
# ggplot(all_data, aes(x, y, color = Mice)) + geom_point() + 
#   facet_wrap(~type) +
#   geom_line() + theme(legend.position = "none")
# ggplot(all_data, aes(x, y, color = type)) + geom_point() + 
#   facet_wrap(~Mice) +
#   geom_line()
# ggplot(zo2_data, aes(x, y, color = Mice)) + geom_point() + geom_line() +
#   theme(legend.position = "none") #+ facet_wrap(~Mice)
