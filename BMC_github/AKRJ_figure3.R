library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(magrittr)
library(ggpubr)
library(gtsummary)
library(scales)


#create "data_fig3.csv"
AKRJ_F <- read_csv("BMC/AKRJ_F_data_NS_S.csv")
AKRJ_F <- AKRJ_F %>% 
  dplyr::filter(mice != "3") %>% 
  dplyr::filter(mice != "7") %>% 
  dplyr::filter(mice != "12") %>% 
  dplyr::filter(mice != "13") %>% 
  dplyr::filter(mice != "14")
C57_F <- read_csv("BMC/C57_F_data_NS_S.csv")
C57_M <- read_csv("BMC/C57_M_data_NS_S.csv")
C57_M <- C57_M %>% 
  dplyr::filter(mice != "36") %>% 
  dplyr::filter(mice != "37") %>% 
  dplyr::filter(mice != "38") %>% 
  dplyr::filter(mice != "39") 

AKRJ_F$mice_name <- paste0("AKRJ_F_", AKRJ_F$mice)
C57_F$mice_name <- paste0("C57_F_", C57_F$mice)
C57_M$mice_name <- paste0("C57_M_", C57_M$mice)

ALL_data_NS_S <- bind_rows(AKRJ_F,C57_F,C57_M) %>% select(-1)
data_fig3 <- ALL_data_NS_S %>% 
  group_by(mice_name) %>% 
  summarize(life_expectency = max(AGE, na.rm = TRUE), life_as_smurf = if (any(STATUS == "S")) first(REMAINTIME[STATUS == "S"]) else NA) %>%
  filter(!is.na(life_expectency)) 

write.csv(data_fig3,"data_fig3.csv", na = "NA")

#plot 1&2 AKRJ

AKRJ_F_fig3 <- data_fig3 %>%
  filter(grepl("^AKRJ_F_", mice_name))

AKRJ_F_y_min <- 0
AKRJ_F_y_max <- max(AKRJ_F_fig3$life_expectency)

AKRJ_F_plot1 <- ggplot(AKRJ_F_fig3, aes(x = "", y = life_expectency)) +
  geom_boxplot(outliers = FALSE, width = 0.3, color = "black", linewidth = 0.5) +
  geom_jitter(width = 0.1, color = "black", size = 0.5) + 
  labs(y = "Life expectency at birth (days)", x = "") +
  ylim(AKRJ_F_y_min, AKRJ_F_y_max)+
  theme_minimal()+
  theme(axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"))

AKRJ_F_stats_plot1 <- AKRJ_F_fig3 %>%
  summarise(Moyenne = mean(life_expectency, na.rm = TRUE),
    Mediane = median(life_expectency, na.rm = TRUE),
    Ecart_type = sd(life_expectency, na.rm = TRUE),
    SEM = sd(life_expectency, na.rm = TRUE) / sqrt(n()))
print(AKRJ_F_stats_plot1)

filtered_data <- AKRJ_F_fig3 %>% filter(life_expectency > 0 & life_as_smurf > 0)
wilcox_test <- wilcox.test(filtered_data$life_expectency, filtered_data$life_as_smurf, paired = TRUE)
p_value <- wilcox_test$p.value
significance <- ifelse(p_value < 0.001, "###", NA)

y_star_position <- 100

AKRJ_F_plot2 <- ggplot(AKRJ_F_fig3, aes(x = "", y = life_as_smurf)) +
  geom_boxplot(outliers = FALSE, width = 0.3, color = "#318CE7", linewidth = 0.5) +
  geom_jitter(width = 0.1, color = "#318CE7", size = 0.5, shape = 17) + 
  labs(y = "Survival time as smurf (days)", x = "") +
  ylim(AKRJ_F_y_min, AKRJ_F_y_max) +
   theme_minimal() +
  theme(axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(x = 1, y = y_star_position, label = significance), inherit.aes = FALSE, 
    color = "black", size = 3, family = "Arial")

AKRJ_F_stats_plot2 <- AKRJ_F_fig3 %>%
  summarise(Moyenne = mean(life_as_smurf, na.rm = TRUE),
            Mediane = median(life_as_smurf, na.rm = TRUE),
            Ecart_type = sd(life_as_smurf, na.rm = TRUE),
            SEM = sd(life_as_smurf, na.rm = TRUE) / sqrt(n()))
print(AKRJ_F_stats_plot2)

#plot 3&4 AKRJ
AKRJ_F_data <- read_csv("BMC/AKRJ_F_data.csv")
AKRJ_F_data <- AKRJ_F_data %>% 
  dplyr::filter(mice != "3") %>% 
  dplyr::filter(mice != "7") %>% 
  dplyr::filter(mice != "12") %>% 
  dplyr::filter(mice != "13") %>% 
  dplyr::filter(mice != "14")

#breakpoint-based animal classification
AKRJ_F_data <- AKRJ_F_data %>% mutate(REMAINTIME = abs(REMAINTIME)) %>% mutate(PRED = ifelse(REMAINTIME>31,"under31","above31"))

#realign mouse age in months
AKRJ_F_data <- AKRJ_F_data %>% mutate(AGE_MONTHS = case_match(AGE,
                                                              c(87, 94, 101) ~ 3.1,
                                                              c(108,115,122) ~ 3.8,
                                                              c(134,141,148) ~ 4.6,
                                                              c(183,190,197) ~ 6.2,
                                                              c(212, 219, 226) ~ 7.2,
                                                              c(238,245,252) ~ 8,
                                                              c(253,260,267) ~ 8.5,
                                                              c(273,280) ~ 9.2,
                                                              c(294) ~ 9.5,
                                                              c(304,311) ~ 10.1,
                                                              c(322,329) ~ 10.7,
                                                              c(331,338) ~ 11,
                                                              c(349, 356) ~ 11.6,
                                                              c(364, 371) ~ 12,
                                                              c(377,384) ~ 12.5,
                                                              c(400,407) ~ 13.2,
                                                              c(422,429) ~ 14,
                                                              c(442) ~ 14.5,
                                                              c(464) ~ 15.2))
AKRJ_F_data <- AKRJ_F_data %>% mutate(AGE_MONTHS = ifelse(DOB == "2018-11-20" & AGE == "287", 9.2, AGE_MONTHS))
AKRJ_F_data <- AKRJ_F_data %>% mutate(AGE_MONTHS = ifelse(DOB == "2018-12-04" & AGE == "287", 9.5, AGE_MONTHS))
AKRJ_F_data$AGE_MONTHS <- style_number(AKRJ_F_data$AGE_MONTHS, digits = 1)

#select variables for PCA
temp1a_AKRJ_F <- AKRJ_F_data %>% dplyr::select(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME, PRED, BW, BT, FAT, FINKCALs, GLY, P1, P3)

#impute missing values to run PCA
temp1b_AKRJ_F <- temp1a_AKRJ_F %>% dplyr::select(!c(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME,PRED))
nb1 <- estim_ncpPCA(temp1b_AKRJ_F,ncp.max=6, scale = FALSE)
nb1
imputed_temp1a_AKRJ_F <- imputePCA(temp1a_AKRJ_F, ncp = 4, scale = TRUE, quali.sup = c(1, 2, 3, 4, 5, 6))

#run and plot PCA
res.pca1_AKRJ_F = PCA(imputed_temp1a_AKRJ_F$completeObs, scale.unit= TRUE, ncp=4, graph=T, quali.sup = c(1, 2, 3, 4, 5, 6))

#classification of individuals into Smurfs and Non-Smurfs based on PCA dimension 1 values
pca1_AKRJ_F <- get_pca_ind(res.pca1_AKRJ_F)
temp1a_pca1_AKRJ_F <- bind_cols(temp1a_AKRJ_F,pca1_AKRJ_F$coord) %>% dplyr::select(!c(Dim.2, Dim.3, Dim.4))
pca1_AKRJ_F_threshold <- temp1a_pca1_AKRJ_F %>% group_by(PRED) %>% dplyr::summarise(mean = mean(Dim.1, na.rm = TRUE), sd= sd(Dim.1, na.rm = TRUE)) %>% 
  filter(PRED == "under31") 
value_pca1_AKRJ_F_threshold <- pca1_AKRJ_F_threshold$mean-(pca1_AKRJ_F_threshold$sd*1.65)
value_pca1_AKRJ_F_threshold
temp1a_pca1_AKRJ_F <- temp1a_pca1_AKRJ_F %>% mutate(STATUS = if_else(Dim.1>value_pca1_AKRJ_F_threshold,"NS","S"))

#AKRJ plot3
temp1a_pca1_AKRJ_F_plot3 <- temp1a_pca1_AKRJ_F
temp1a_pca1_AKRJ_F_plot3 <- temp1a_pca1_AKRJ_F_plot3 %>%
  mutate(AGE_MONTHS = factor(AGE_MONTHS, levels = unique(AGE_MONTHS)))
significance_results <- temp1a_pca1_AKRJ_F_plot3 %>%
  group_by(AGE_MONTHS) %>%
  summarize(groups_present = n_distinct(STATUS), 
            p_value = ifelse(groups_present == 2,
                             wilcox.test(REMAINTIME ~ STATUS, data = cur_data())$p.value,NA)) %>%
  mutate(significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ NA_character_)) %>% filter(!is.na(significance))

significance_results <- significance_results %>%
  left_join(temp1a_pca1_AKRJ_F_plot3 %>%
              group_by(AGE_MONTHS) %>%
              summarize(y_max = max(REMAINTIME, na.rm = TRUE)), by = "AGE_MONTHS") %>%
  mutate(bar_start = y_max + 10, bar_end = y_max + 15, text_position = y_max + 20)

AKRJ_F_plot3 <- temp1a_pca1_AKRJ_F_plot3 %>%
  ggplot(aes(x = AGE_MONTHS, y = REMAINTIME, color = STATUS, shape = STATUS)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(1), linewidth = 0.3) + 
  geom_jitter(position = position_dodge(1), size = 0.5) + 
  scale_color_manual(values = c("black", "#318CE7"), labels = c("Non-Smurfs", "Smurfs")) +
  scale_shape_manual(values = c(16, 17), labels = c("Non-Smurfs", "Smurfs")) +
  labs(y = "Survival time (days)", x = "Age (months)", color = "", shape = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold")) +
  scale_x_discrete(breaks = levels(temp1a_pca1_AKRJ_F_plot3$AGE_MONTHS)[seq(1, length(levels(temp1a_pca1_AKRJ_F_plot3$AGE_MONTHS)), by = 3)])+
  # Ajouter les barres reliant les groupes
  geom_segment(data = significance_results, 
               aes(x = as.numeric(AGE_MONTHS) - 0.2, 
                   xend = as.numeric(AGE_MONTHS) + 0.2,
                   y = bar_start, 
                   yend = bar_start),
               inherit.aes = FALSE, color = "black") +
  # Ajouter les étoiles au-dessus des barres
  geom_text(data = significance_results, 
            aes(x = as.numeric(AGE_MONTHS), 
                y = text_position, 
                label = significance),
            inherit.aes = FALSE, color = "black", size = 5)

#AKRJ plot4
temp1a_pca1_AKRJ_F_plot4 <- temp1a_pca1_AKRJ_F %>% dplyr::group_by(AGE_MONTHS, STATUS) %>% dplyr::summarise(n = n()) %>% dplyr::mutate(PROP = (n/sum(n))*100) 
temp1a_pca1_AKRJ_F_plot4 <- temp1a_pca1_AKRJ_F_plot4 %>% transform(AGE_MONTHS = as.numeric(AGE_MONTHS))
data <- temp1a_pca1_AKRJ_F_plot4 %>% dplyr::filter(STATUS == "S")
exp_model <- nls(PROP ~ a * exp(b * AGE_MONTHS), data = data, start = list(a = 100, b = -0.1))
data <- data %>% mutate(Fitted = predict(exp_model))
ss_total <- sum((data$PROP - mean(data$PROP))^2)  # Somme des carrés totaux
ss_residual <- sum((data$PROP - data$Fitted)^2)  # Somme des carrés résiduels
r_squared <- 1 - (ss_residual / ss_total)
r_squared <- round(r_squared, 3)# Calcul du R^2

AKRJ_F_plot4 <- ggplot(data, aes(x = AGE_MONTHS, y = PROP)) +
  geom_point(color = "#318CE7", size = 1, shape = 17) +
  geom_line(aes(y = Fitted), color = "#318CE7", size = 1, linetype = 1) +
  labs(x = "Age (months)", y = "Proportion of smurfs (%)") +
  annotate("text", x = max(data$AGE_MONTHS) * 0.4, y = max(data$PROP) * 0.9,
           label = paste0("R² = ", r_squared),
           hjust = 0, size = 3, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"))

#compile plot1,2,3,4
layout <- rbind(c(1,2,3,3,3,4,4))

AKRJ_F_figure3 <- grid.arrange(
  AKRJ_F_plot1, 
  AKRJ_F_plot2, 
  AKRJ_F_plot3, 
  AKRJ_F_plot4, 
  layout_matrix = layout)





