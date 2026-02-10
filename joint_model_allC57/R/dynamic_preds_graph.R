##############################
# ELEGANT GGPLOT FOR DYNAMIC PREDICTIONS
##############################

library(ggplot2)
library(scales)
library(data.table)

# Assuming the data preparation code from dynamic_preds.R has been run:
# t0=1000
# alive_t0 = c57[AGE_DEATH>t0, (unique(mouse))]
# ND <- c57[mouse %in% alive_t0 & AGE < t0]
# ND$death <- 0
# ND$AGE_DEATH <- t0
# predSurv <- predict(jointFit_c57, newdata = ND, process = "event",
#                     times = seq(t0, t0+9*30, length.out = 500),
#                     return_newdata = TRUE)
# predSurv_sht <- predSurv[, AGE_DEATH:upp_CIF]
# t50_pred <- predSurv[, .SD[which.min(abs(pred_CIF - 0.5))], by = mouse]
# setnames(t50_pred, "AGE_DEATH", "AGE_T50")
# t50_pred[c57, on=.(mouse), AGE_DEATH:=AGE_DEATH]

create_survival_plot <- function(data) {
  # Fit linear model for statistics
  model_lm <- lm(AGE_DEATH ~ AGE_T50, data = data)
  model_summary <- summary(model_lm)
  r_squared <- round(model_summary$r.squared, 3)
  p_value <- round(model_summary$coefficients[2,4], 4)
  
  # Create the plot
  p_survival <- ggplot(data, aes(x = AGE_T50, y = AGE_DEATH)) +
    geom_point(alpha = 0.7, size = 2.5, color = "#4A90E2") +
    geom_smooth(method = "lm", se = TRUE, color = "#F28322", 
                fill = "#F28322", alpha = 0.2, linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", linewidth = 0.8) +
    labs(
      x = "Predicted T50 (days)", 
      y = "Actual Age at Death (days)",
      title = "Predicted vs Actual Survival Times",
      subtitle = paste0("R² = ", r_squared, ", p-value = ", 
                       if(p_value < 0.001) "< 0.001" else p_value)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray60"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    ) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = comma_format())
  
  return(list(plot = p_survival, model = model_summary))
}

# Usage example (uncomment when data is available):
# result <- create_survival_plot(t50_pred)
# print(result$plot)
# print(result$model)

# Save plot function
save_survival_plot <- function(data, filename = "survival_prediction_plot.pdf", 
                              width = 8, height = 6) {
  result <- create_survival_plot(data)
  ggsave(filename, result$plot, width = width, height = height)
  cat("Plot saved to:", filename, "\n")
  return(result$model)
}

# Enhanced version with additional styling options
create_enhanced_survival_plot <- function(data, color_points = "#4A90E2", 
                                        color_line = "#F28322",
                                        show_identity_line = TRUE) {
  model_lm <- lm(AGE_DEATH ~ AGE_T50, data = data)
  model_summary <- summary(model_lm)
  r_squared <- round(model_summary$r.squared, 3)
  p_value <- round(model_summary$coefficients[2,4], 4)
  
  p <- ggplot(data, aes(x = AGE_T50, y = AGE_DEATH)) +
    geom_point(alpha = 0.7, size = 2.5, color = color_points) +
    geom_smooth(method = "lm", se = TRUE, color = color_line, 
                fill = color_line, alpha = 0.2, linewidth = 1)
  
  if (show_identity_line) {
    p <- p + geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                        color = "gray50", linewidth = 0.8)
  }
  
  p <- p +
    labs(
      x = "Predicted T50 (days)", 
      y = "Actual Age at Death (days)",
      title = "Predicted vs Actual Survival Times",
      subtitle = paste0("R² = ", r_squared, ", p-value = ", 
                       if(p_value < 0.001) "< 0.001" else p_value)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray60"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = comma_format())
  
  return(list(plot = p, model = model_summary))
}