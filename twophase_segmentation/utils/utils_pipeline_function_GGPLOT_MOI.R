#' use_me: boolean: specify whether to cancel out "mixed effects", i.e. to remove the 
#' effect of the mice (mouse-specific intercept and slope)
run_pipeline <- function(y_var, data, use_me = NULL,
                         file_path = "tmp/", x_var = "DBD",
                         control = seg.control(),
                         seed = FALSE) {
  ## use_me: whether to cancel out mixed effect. If NULL, this is determined by
  ## the method.
  ## alpha = 0 allows the breakpoint to be anywhere in range(data$x)
  
  # Random seed
  # if (is.numeric(seed)) {
  #   set.seed(seed)
  # } else if (is.boolean(seed) && seed) {
  #   set.seed(9876)
  # }
  
  rawdata <- tibble(x = data[[x_var]],
                    y = data[[y_var]],
                    Mice = as.factor(data$mice)) %>% 
    remove_missing(na.rm = TRUE)
  
  print("------------------------------------------------------")
  print(paste("Running slme on variable", y_var))
  print("------------------------------------------------------")
  
  ## Zeroing out data ------------------------------------------------------
  if (is.null(use_me)) {
    ### Estimate if it is relevant to consider effects of individuals ("mixed effects")
    use_me <- is_me_necessary(y_var, data)
    if (is.null(use_me)) {
      use_me <- FALSE
    }
  }
  
  ### Zero-out the effect of the mice in the data
  if (use_me) {
    print(paste("Zeroing out the mice effects for variable", y_var)) 
    # zo_data <- rawdata %>% zero_out_lm() %>% arrange(x)
    zo_data <- rawdata %>% zero_out_lme(use_fe = FALSE, filter_mice = TRUE) %>% arrange(x)
    y_lab <- paste(y_var, "(modified for random effects)")
  } else {
    print(paste("Not zeroing out the mice effects for variable", y_var)) 
    zo_data <- rawdata %>% arrange(x)
    y_lab <- y_var
  }
  
  print("------------------------------------------------------")
  
  ## Segmented method ------------------------------------------------
  ### Running selgmented ---------------------------------------------
  # assign in global envir, as necessary for running selgmented
  ## Add small perturbation along x axis to avoid computation problems caused by duplicates
  if (anyDuplicated(zo_data$x)) {
    zo_data$x <- zo_data$x + rnorm(nrow(zo_data), 0, 0.01)
    zo_data <- zo_data %>% arrange(x)
  }
  assign("zo_data_global", zo_data, envir = .GlobalEnv)
  lm1 <- lm(y ~ x + 1, data = zo_data_global)
  output <- capture.output(
    slm1 <- selgmented(lm1, type = "score", control = control, bonferroni = FALSE,
                       alpha = 0.05, Kmax = 2)
    # because alpha.adj = alpha / Kmax
  )
  rm(zo_data_global, envir = global_env()) # rm the variable created in the global env.
  ### Capture output ---------------------------------------------------
  print(output)
  tests <- str_split(output[3], "   ")[[1]] %>% 
    str_remove(pattern = "=.*") %>% str_c("= ")
  p_values <- slm1$selection.psi$pvalues[1:2] %>% signif(digits = 4)
  tests <- str_c(tests, p_values)
  
  ## Plotting model  -------------------------------------------------------
  if (slm1$selection.psi$npsi > 0) {
    xseq <- c(min(zo_data$x), confint(slm1)[, 1], max(zo_data$x))
    xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200), 
               confint(slm1)[, 1]) %>% sort()
  } else {
    xseq <- c(min(zo_data$x), max(zo_data$x))
    xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200)) %>% sort()
  }
  pred <- predict(slm1, data.frame(x = xseq))
  
  
  ############################################################################
  ########################final figure edition################################
  #pdf(file = paste0(file_path, y_var, ".pdf"),
 #     width = 8, height = 6)

  
 #############################################################################
  # Base plot with data points
  # Ensure zo_data, xseq, xseq2, pred, slm1, confint(slm1), and other variables are properly defined
  # and contain the expected data structure.
  
  p <- ggplot(zo_data, aes(x = x, y = y)) +  # Correctly map x and y columns
    geom_point(color = "grey") +            # Use zo_data for points, color set outside aes()
    labs(x = "Age (days)", y = y_lab, title = output[5]) +  # Add labels
    theme_minimal()
  
   # Add prediction line
  p <- p +
    geom_line(data = data.frame(x = xseq, y = pred), aes(x = x, y = y), color = "red", size = 1)
  
  # Add confidence intervals
  pred_slm1 <- predict(slm1, newdata = data.frame(x = xseq2), se.fit = TRUE)
  crit_val <- qt(0.975, df = slm1$df.residual)
  
  ci <- data.frame(
    x = xseq2,
    upper = pred_slm1$fit + crit_val * pred_slm1$se.fit,
    lower = pred_slm1$fit - crit_val * pred_slm1$se.fit
  )
  p <- p +
    geom_ribbon(data = ci, aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE, alpha = 0.2, fill = "blue") 
  
  # Save the plot
  file_path <- paste0(file_path, y_var, ".pdf")
  ggsave(filename = file_path, plot = p, width = 8, height = 6)
  
  
  

  ############################################################################
  #dev.off()
  
  print("------------------------------------------------------")
  print("File saved to:")
  print(file_path)
  print("------------------------------------------------------")
  
  invisible(slm1)
}

#' @param y_var names of the y varible to use
#' @param data dataset containing the y variable and the time before death as the x variable. It is expected that the time before deaths variable has the name "DBD" and that the variable of the individual has the name "mice".
#' @return a boolean: whether it is estimate that individuals have 
is_me_necessary <- function(y_var, data) {
  ## Standardize dataset
  data_tmp <- data %>% 
    transmute(x = DBD,
              y = !!rlang::sym(y_var), 
              Mice = as.factor(mice)) %>% 
    remove_missing(na.rm = TRUE)
  
  ## Remove mice that have too few observations to perform computation: 3 or less data points
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>%
    filter(n <= 1) %>%
    '$'(Mice)
  
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>%
    mutate(Mice = droplevels(Mice))
  
  ## Run linear model with mixed effect of Mice onto the slope and intercept of the y variable
  lme1 = lme(y ~ x, 
             # random = ~1 + x | Mice, ## throws convergence error
             random = list(Mice = pdDiag(~ 1 + x)), ## random effects assumed independent
             data = data)
  
  ## Run segmented linear model with similar mixed effects.
  ## Since segmented.lme is prone to computational errors, we use tryCatch and 
  ## set use_me to FALSE if an error was returned
  slme1 <- NULL
  for (ind in 1:10) {
    if (is.null(slme1)) {
      slme1 <- tryCatch(
        segmented.lme(lme1, ~x, 
                      # random = list(Mice = ~1 + x + U | Mice), ## throws convergence error
                      random = list(Mice = pdDiag(~1 + x + U)), ## random effects assumed independent
                      data = data),
        error = function(e) {
          warning("Warning: is_me_necessary ran into an error. use_me set to FALSE.");
          NULL
        })
    }
  }
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

run_pipeline_sex <- function(y_var, data, use_me = NULL,
                             file_path = "tmp/", x_var = "DBD",
                             control = seg.control(),
                             seed = FALSE, fe_var = "sex") {
  ## use_me: whether to cancel out mixed effect. If NULL, this is determined by
  ## the method.
  ## alpha = 0 allows the break-point to be anywhere in range(data$x)
  
  # Random seed
  # if (is.numeric(seed)) {
  #   set.seed(seed)
  # } else if (is.boolean(seed) && seed) {
  #   set.seed(9876)
  # }
  
  rawdata <- tibble(x = data[[x_var]],
                    y = data[[y_var]],
                    Mice = as.factor(data$mice),
                    fe_var = data[[fe_var]]) %>% 
    remove_missing(na.rm = TRUE)
  
  print("------------------------------------------------------")
  print(paste("Running slme on variable", y_var))
  print("------------------------------------------------------")
  
  ## Zeroing out data ------------------------------------------------------
  if (is.null(use_me)) {
    ### Estimate if it is relevant to add random effects of individuals
    use_me <- is_me_necessary_sex(y_var, data, fe_var = fe_var)
    if (is.null(use_me)) {
      use_me <- FALSE
    }
  }
  ### Zero-out the effect of the mice in the data
  if (use_me) {
    print(paste("Zeroing out the mice effects for variable", y_var)) 
    print("------------------------------------------------------")
    # zo_data <- rawdata %>% zero_out_lm() %>% arrange(x)
    zo_data <- rawdata %>% zero_out_lme(use_fe = TRUE) %>% arrange(x)
    y_lab <- paste(y_var, "(modified for random effects)")
  } else {
    print(paste("Not zeroing out the mice effects for variable", y_var)) 
    print("------------------------------------------------------")
    zo_data <- rawdata %>% arrange(x)
    y_lab <- y_var
  }
  
  ## Segmented method ------------------------------------------------
  ### Running selgmented ---------------------------------------------
  # assign in global envir, as necessary for running selgmented
  ## Add small perturbation along x axis to avoid computation problems caused by duplicates
  # if (anyDuplicated(zo_data$x)) {
  #   set.seed(345)
  #   zo_data$x <- zo_data$x + rnorm(nrow(zo_data), 0, 0.01)
  #   zo_data <- zo_data %>% arrange(x)
  # }
  
  assign("zo_data_global", zo_data, envir = .GlobalEnv)
  lm1 <- lm(y ~ x + 1 + fe_var, data = zo_data_global)
  output <- capture.output(
    slm1 <- selgmented(lm1, 
                       seg.Z = ~x,
                       type = "score", control = control, bonferroni = FALSE,
                       alpha = 0.05, Kmax = 2)
    # because alpha.adj = alpha / Kmax
  )
  rm(zo_data_global, envir = global_env()) # rm the variable created in the global env.
  ### Capture output ---------------------------------------------------
  print(output)
  tests <- str_split(output[3], "   ")[[1]] %>% 
    str_remove(pattern = "=.*") %>% str_c("= ")
  p_values <- slm1$selection.psi$pvalues[1:2] %>% signif(digits = 4)
  tests <- str_c(tests, p_values)
  
  ## Plotting model  -------------------------------------------------------
  if (slm1$selection.psi$npsi > 0) {
    xseq <- c(min(zo_data$x), confint(slm1)[, 1], max(zo_data$x))
    xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200), 
               confint(slm1)[, 1]) %>% sort()
  } else {
    xseq <- c(min(zo_data$x), max(zo_data$x))
    xseq2 <- c(seq(min(zo_data$x), max(zo_data$x), length.out = 200)) %>% sort()
  }
  pred <- predict(slm1, data.frame(x = xseq, fe_var = "female"))
  
  pdf(file = paste0(file_path, y_var, ".pdf"),
      width = 8, height = 6)
  plot(zo_data$x, zo_data$y, col = "grey",
       xlab = "Age (days)",
       ylab = y_lab)
  title(output[5])
  usr <- c(range(zo_data$x), range(zo_data$y))
  k <- 10; h <- (usr[4] - usr[3]) / k; y <- usr[3] + h
  v <- (usr[2] - usr[1]) / k; x <- usr[1] + v * 2
  lines(pred, x = xseq, col = "red", lwd = 2)
  lines(pred + slm1$coefficients["fe_varmale"], x = xseq, col = "red", lwd = 2)
  if (slm1$selection.psi$npsi > 0) {
    lines(slm1, term = "x", col = 2, k = 20, lwd = 2)
    for (ind in 1:nrow(confint(slm1))) {
      abline(v = confint(slm1)[ind], lty = 2, col = "lightblue", lwd = 2)
    }
  }
  ### Textual elements ------
  text(x, y + h * 1.5, paste("adjusted R^2 linear =",
                             signif(summary(lm1)$adj.r.squared, digits = 2)))
  p_smaller <- ifelse(p_values < c(0.025, 0.05), "<", ">=")
  text(x, y + h / 2, paste(tests[1], p_smaller[1], "0.025"))
  text(x, y, paste(tests[2], p_smaller[2], "0.05"))
  if (slm1$selection.psi$npsi > 0) { 
    text(x, y + h, paste("adjusted R^2 segmented =",
                         signif(summary(slm1)$adj.r.squared, digits = 2)))
    for (ind in 1:nrow(confint(slm1))) {
      text(confint(slm1)[ind], y - h / 2, signif(confint(slm1)[ind], digits = 3))
    }
  }
  ### CI ------
  pred_slm1 <- predict(slm1, data.frame(x = xseq2, fe_var = "female"),
                       se.fit = TRUE)
  crit_val <- qt(0.975, df = slm1$df.residual)
  lines(pred_slm1$fit + crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
  lines(pred_slm1$fit - crit_val * pred_slm1$se.fit, x = xseq2, lty = 2)
  lines(pred_slm1$fit + crit_val * pred_slm1$se.fit +
          slm1$coefficients["fe_varmale"], x = xseq2, lty = 2)
  lines(pred_slm1$fit - crit_val * pred_slm1$se.fit + 
          slm1$coefficients["fe_varmale"], x = xseq2, lty = 2)
  dev.off()
  
  print("------------------------------------------------------")
  print("File saved to:")
  print(paste0(file_path, y_var, ".pdf"))
  print("------------------------------------------------------")
  
  invisible(slm1)
}

is_me_necessary_sex <- function(y_var, data, fe_var = "sex") {
  ## Standardize dataset
  data_tmp <- data %>% 
    transmute(x = DBD,
              y = !!rlang::sym(y_var), 
              Mice = as.factor(mice), 
              fe_var = data[[fe_var]]) %>% 
    remove_missing(na.rm = TRUE)
  
  ## Remove mice that have too few observations to perform computation: 3 or less data points
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 1) %>% 
    '$'(Mice)
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  
  ## Run linear model:
  ## - with mixed effect of Mice on the intercept and slope
  lme1 <- lme(y ~ x + fe_var,
              # random = ~1 + x | Mice, ## assume random effects can be correlated
              random = list(Mice = pdDiag(~ 1 + x)), ## random effects assumed independent
              data = data)
  
  ## Run segmented linear model with similar mixed effects.
  ## Since segmented.lme is prone to computational errors, we use tryCatch and 
  ## set use_me to FALSE if an error was returned
  slme1 <- NULL
  for (ind in 1:10) {
    if (is.null(slme1)) {
      slme1 <- tryCatch(
        segmented.lme(
          lme1, 
          seg.Z = ~x, 
          # random = list(Mice = ~ 1 + x + U | Mice), ## assume random effects can be correlated
          random = list(Mice = pdDiag(~ 1 + x + U)), ## assume random effects are uncorrelated
          data = data),
        error = function(e) NULL)
    }
  }
  
  if (is.null(slme1)) {
    use_me <- "NULL"
    warning("Warning: is_me_necessary ran into an error. use_me set to FALSE.")
  } else {
    assign("data_global", data, envir = .GlobalEnv)
    lm1 <- lm(y ~ x + fe_var, data = data_global)
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

