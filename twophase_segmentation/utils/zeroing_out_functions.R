### Main functions  ----
zero_out_slope <- function(data, x_name = "x", y_name = "y", psi_init = -50) {
  data_tmp <- data %>% 
    dplyr::select(one_of(x_name, y_name, "Mice")) %>% 
    arrange(Mice) %>% 
    drop_na() %>% 
    setNames(c("x", "y", "Mice"))
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 3) %>% 
    '$'(Mice)
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  lme1 <- lme(y ~ x, random = list(Mice = pdDiag(~ 1 + x)), data = data,
              control = lmeControl(msMaxIter = 1000, msMaxEval = 1000))
  slme1 <- segmented.lme(lme1, ~ x, random = list(Mice = pdDiag(~ 1 + x)),
                         control = seg.control(n.boot = 10, it.max = 100,
                                               display = TRUE),
                         data = data, psi = psi_init) ## Careful! no bootstrap
  data_mod <- data %>% 
    mutate(y = zero_out_slope_df(levels(Mice), data, slme1) %>% 
             unlist %>% as.vector)
  return(data_mod)
}
zero_out_slope_diff <- function(rawdata, x_name = "x", y_name = "y", psi_init = -50) {
  data_tmp <- rawdata %>% 
    dplyr::select(one_of(x_name, y_name, "Mice")) %>% 
    arrange(Mice) %>% 
    drop_na() %>% 
    setNames(c("x", "y", "Mice"))
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 3) %>% 
    '$'(Mice)
  print("Removed the following Mice with too few observations:")
  print(data_tmp %>% filter(Mice %in% mice_with_too_few_obs) %>%
          group_by(Mice) %>%  tally(name = "n_obs"))
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  lme1 <- lme(y ~ x, random = list(Mice = pdDiag(~ 1 + x)), data = data,
              control = lmeControl(msMaxIter = 1000, msMaxEval = 1000))
  slme1 <- segmented.lme(lme1, ~ x, random = list(Mice = pdDiag(~ 1 + x + U)),
                         control = seg.control(n.boot = 3, it.max = 100, display = TRUE),
                         data = data, psi = psi_init)
  data_mod <- data %>% 
    mutate(y = zero_out_slope_diff_df(levels(Mice), data, slme1) %>%
             unlist %>% as.vector)
  return(data_mod)
}
zero_out_lm <- function(rawdata, x_name = "x", y_name = "y") {
  data_tmp <- rawdata %>% 
    dplyr::select(one_of(x_name, y_name, "Mice")) %>% 
    arrange(Mice) %>% 
    drop_na() %>% 
    setNames(c("x", "y", "Mice"))
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 2) %>% 
    '$'(Mice)
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  ## overall effect
  fit <- lm(y ~ x, data = data) %>% coef()
  slope_global = fit[2]; intercept_global = fit[1]
  ## group effect
  group_effects_df <- data %>% group_by(Mice) %>% 
    group_map(~lm(y ~ x, data = .x)) %>% 
    purrr::map_dfr(coef) %>% 
    dplyr::rename(intercept = `(Intercept)`, slope = x) %>% 
    mutate(Mice = data$Mice %>% unique())
  data_mod <- left_join(data, group_effects_df, by = "Mice") %>%
    transmute(x, y = y - intercept - x * slope +
                intercept_global + slope_global * x, Mice)
  return(data_mod)
}
zero_out_lm_sex <- function(rawdata, x_name = "x", y_name = "y") {
  data_tmp <- rawdata %>% 
    dplyr::select(one_of(x_name, y_name, "Mice", "sex")) %>% 
    arrange(Mice) %>% 
    drop_na() %>% 
    setNames(c("x", "y", "Mice", "sex"))
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 2) %>% 
    '$'(Mice)
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  ## overall effect
  fit <- lm(y ~ x, data = data) %>% coef()
  slope_global = fit[2]; intercept_global = fit[1]
  ## group effect
  group_effects_df <- data %>% group_by(Mice) %>% 
    group_map(~lm(y ~ x, data = .x)) %>% 
    purrr::map_dfr(coef) %>% 
    dplyr::rename(intercept = `(Intercept)`, slope = x) %>% 
    mutate(Mice = data$Mice %>% unique())
  data_mod <- left_join(data, group_effects_df, by = "Mice") %>%
    transmute(x, y = y - intercept - x * slope +
                intercept_global + slope_global * x, Mice)
  return(data_mod)
}


zero_out_lme_old <- function(rawdata, x_name = "x", y_name = "y", psi_init = -50) {
  data_tmp <- rawdata %>% 
    dplyr::select(one_of(x_name, y_name, "Mice")) %>% 
    arrange(Mice) %>% 
    drop_na() %>% 
    setNames(c("x", "y", "Mice"))
  mice_with_too_few_obs <- data_tmp %>% group_by(Mice) %>% tally() %>% 
    filter(n <= 2) %>% 
    '$'(Mice)
  data <- data_tmp %>% 
    filter(!(Mice %in% mice_with_too_few_obs)) %>% 
    mutate(Mice = droplevels(Mice))
  group_effects_df <- lmer(y ~ x + (1|Mice), data = data) %>% 
    coef() %>% '$'(Mice) %>% 
    dplyr::rename(intercept = `(Intercept)`, slope = x) %>% 
    mutate(Mice = data$Mice %>% unique())
  ### NOT WRITTEN YET
  group_effects_df <- data %>% group_by(Mice) %>% 
    group_map(~lme(y ~ x, data = .x)) %>% 
    purrr::map_dfr(coef) %>% 
    dplyr::rename(intercept = `(Intercept)`, slope = x) %>% 
    mutate(Mice = data$Mice %>% unique())
  data_mod <- left_join(data, group_effects_df, by = "Mice") %>%
    transmute(x, y = y - intercept - x * slope, Mice)
  return(data_mod)
}

### Utility functions ----
zero_out_slope_diff_tmp1 <- function(id, x, y, slme) {
  coef <- slme$lme.fit$coefficients$random$id[id, ]
  G0 <- slme1$lme.fit$coefficients$fixed[4]
  y_mod <- y - (coef[1] + coef[2] * x + coef[3] * pmax(x - G0, 0)) 
  return(unname(y_mod))
}
zero_out_slope_diff_tmp2 <- function(id, data, slme) {
  zero_out_slope_diff_tmp1(id, data %>% filter(Mice == id) %>% '$'(x), 
                           data %>% filter(Mice == id) %>% '$'(y),
                           slme)
}
zero_out_slope_diff_df <- Vectorize(zero_out_slope_diff_tmp2, vectorize.args = "id")

zero_out_slope_tmp1 <- function(id, x, y, slme) {
  coef <- slme$lme.fit$coefficients$random$id[id, ]
  y_mod <- y - (coef[1] + coef[2] * x)
  return(unname(y_mod))
}
zero_out_slope_tmp2 <- function(id, data, slme) {
  zero_out_slope_tmp1(id, data %>% filter(Mice == id) %>% '$'(x), 
                      data %>% filter(Mice == id) %>% '$'(y),
                      slme)
}
zero_out_slope_df <- Vectorize(zero_out_slope_tmp2, vectorize.args = "id")
## Different version: TODO check what the difference is with the other version
# zero_out_slope_diff <- function(data) {
#   data <- data %>% arrange(Mice)
#   lme1 <- lme(y ~ x, random = list(Mice = pdDiag(~ 1 + x)), data = data)
#   slme1 <- segmented.lme(lme1, ~ x, random = list(Mice = pdDiag(~ 1 + x + U)),
#                          data = data,
#                          control = seg.control(n.boot = 0))
#   data_mod <- data %>%
#     arrange(Mice) %>%
#     mutate(y_mod = zero_out_slope_diff_df(levels(Mice), data, slme1) %>% unlist)
#   return(data_mod)
# }

zero_out_lme = function(data, use_fe = FALSE, filter_mice = FALSE) {
  
  ## To prevent convergence errors, we have to remove Mice with too few observations
  ## from the zeroing out process
  
  if (filter_mice) {
    mice_with_too_few_obs <- data %>% group_by(Mice) %>% tally() %>%
      filter(n <= 1) %>%
      '$'(Mice)
    data = data %>%
      filter(!(Mice %in% mice_with_too_few_obs)) %>%
      mutate(Mice = droplevels(Mice))
  } else {
    mice_with_too_few_obs = NULL
  }
  
  
  if (use_fe) {
    lme1 <- lme(y ~ x + fe_var,
                random = ~1 + x | Mice, ## throws convergence error
                # random = list(Mice = pdDiag(~1 + x)), ## random effects assumed independent
                data = data)
  } else {
    lme1 <- lme(y ~ x, 
                random = ~1 + x | Mice, ## throws convergence error
                # random = list(Mice = pdDiag(~1 + x)), ## random effects assumed independent
                data = data)
  }
  
  # plcot(data$x, data$y, col = data$Mice)
  ## Same with ggplot
  # ggplot(data, aes(x, y, color = Mice)) + geom_point() + geom_line()
  
  ## Try catch to prevent errors, trying 10 times
  # for (ind_try in 1:10) {
  #   set.seed(ind_try)
  #   slme1 = try({
  #     segmented.lme(lme1,
  #                   seg.Z = ~x,
  #                   # random = list(Mice = ~ 1 + x + U | Mice), ## throws convergence error
  #                   random = list(Mice = pdDiag(~ 1 + x + U)), ## random effects assumed independent
  #                   data = data,
  #                   control = seg.control(n.boot = 100, display = "TRUE", tol = 1e-4))
  #   })
  #   if (class(slme1) != "try-error") {
  #     print("Success!")
  #     break
  #   }
  # }
  set.seed(123)
  slme1 = segmented.lme(lme1,
                        seg.Z = ~x,
                        # random = list(Mice = ~ 1 + x + U | Mice), ## throws convergence error
                        random = list(Mice = pdDiag(~ 1 + x + U)), ## random effects assumed independent
                        data = data,
                        control = seg.control(n.boot = 100))
  
  ## There should be 1 BP in the lme estimate
  stopifnot(length(unique(slme1$psi.i)) == 1)
  breakpoint = unique(slme1$psi.i)
  
  ## Get random effects
  rand_effects_slme = ranef(slme1$lme.fit)
  
  ## Add mice which where removed, setting the random effects to 0
  if (length(mice_with_too_few_obs) > 0) {
    for (mice_ in mice_with_too_few_obs) {
      ## Add vector c("(Intercept)" = 0, "x" = 0, "U" = 0) to the matrix rand_effects_slme
      rand_effects_slme = rbind(rand_effects_slme, c("(Intercept)" = 0, "x" = 0, "U" = 0))
      ## Set the last row names to mice_
      rownames(rand_effects_slme)[nrow(rand_effects_slme)] = mice_
    }
  }
  
  ## Compute y_corrected for random effects for each Mice
  data_2 = tibble()
  for (mice_ in data$Mice %>% unique()) {
    data_mice = data %>% filter(Mice == mice_)
    rand_effects_mice = rand_effects_slme[mice_, ]
    data_mice$y_corrected = data_mice$y - 
      rand_effects_mice[["(Intercept)"]] -
      rand_effects_mice[["x"]] * data_mice$x - 
      rand_effects_mice[["U"]] * pmax(data_mice$x - breakpoint, 0)
    
    data_2 = bind_rows(data_2, data_mice)
  }
  data_2 = data_2 %>% transmute(x = x, y = y_corrected, 
                                Mice = Mice)
  if (use_fe) {
    data_2 = data_2 %>% mutate(fe_var = data$fe_var)
  } 
  return(data_2)
}


