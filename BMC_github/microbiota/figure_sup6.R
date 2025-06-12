library(readxl)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(patchwork)
library(gridExtra)
library(kSamples)
library(dplyr)
library(tidyverse)
library(rstatix)


metadata <- read_excel("BMC/microbiota/metadata_metagenomic_phyloseq_analysis.xlsx", 
                                                     col_types = c("text", "text", "text", 
                                                                   "text", "text", "text", "text", "date", 
                                                                   "text", "numeric", "numeric", "text", 
                                                                   "numeric", "text", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric"))
metadata <- metadata[,-1]

abundance <- read_excel("BMC/microbiota/relative_abondance_metagenomic_phyloseq_analysis.xlsx")
abundance_long <- abundance %>% pivot_longer(cols = -Family, names_to = "Sample", values_to = "Abundance")

data <- left_join(abundance_long, metadata, by = join_by(Sample == Name))
data <- data %>% select(Family,Sample,Abundance,Group_bis,Group)
data <- data %>% rename(Age = Group_bis, Status = Group)
data <- data %>% dplyr::filter(Status != "NA") 
unique(data$Family)

data_sum <- data %>% group_by(Sample) %>% summarise(tot = sum(Abundance))

temp1 <- data %>% filter(Family == "Eubacteriales incertae sedis")
temp1 <- temp1 %>%
  mutate(Group_age = ifelse(Age %in% c("M10_12", "M9-10"), "9_12", "6_7"))


plot1 <- ggplot(temp1, aes(x = Group_age, y = Abundance, pattern = Group_age)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(1)) +
  geom_boxplot_pattern(position = position_dodge(1), 
                       color = "black", pattern_fill = "white", 
                       pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.05, 
                       pattern_key_scale_factor = 0.6,
                       outlier.shape = NA) +
  geom_jitter(position = position_dodge(1), size = 0.5, show.legend = FALSE) +  # Supprime la légende des points
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),  # Supprime l'étiquette X ici
        legend.position = "none",  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold", size = 12)) +  # Taille du titre Y
  labs(title = "Eubacteriales incertae sedis", y = "Relative abundance")

plot2 <- ggplot(temp1, aes(x = Group_age, y = Abundance, pattern = Group_age, fill = Status)) +
  geom_boxplot(aes(fill = Status), outlier.shape = NA, position = position_dodge(1)) +
  scale_fill_manual(name = "Status", values = c("white", "#318CE7")) +
  geom_boxplot_pattern(position = position_dodge(1), 
                       color = "black", pattern_fill = "white", 
                       pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.025, 
                       pattern_key_scale_factor = 0.6,
                       outlier.shape = NA) +
  geom_jitter(aes(color = Status), position = position_dodge(1), size = 0.5, show.legend = FALSE) +  # Supprime la légende des points
  scale_color_manual(name = "Status", values = c("black", "#318CE7")) +
  guides(
    pattern = guide_legend(title = "Group of age", override.aes = list(fill = "white"), order = 1),
    fill = guide_legend(title = "Status", override.aes = list(pattern = "none"), order = 2)  # "Status" sous "Group of age"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),  # Supprime l'étiquette X ici aussi
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold", size = 12)) +  # Taille du titre Y
  labs(y = "")

# Compiler les graphiques avec un titre commun pour l'axe des X
AKRJ_F_figure_sup6 <- (plot1 + plot2) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(caption = "Age (months)") & 
  theme(plot.caption = element_text(size = 12, face = "bold", hjust = 0.3))  # Texte plus gros et décalé à gauche

AKRJ_F_figure_sup6

#statistics
temp1$Group_age <- as.factor(temp1$Group_age)
Stat1_plot1 <- temp1 %>% wilcox_test(Abundance ~ Group_age) %>% add_significance()
Stat1_plot1
Stat1_plot2 <- temp1 %>% group_by(Group_age) %>% wilcox_test(Abundance ~ Status) %>% add_significance()
Stat1_plot2
Stat2_plot2 <- temp1 %>% group_by(Status) %>% wilcox_test(Abundance ~ Group_age) %>% add_significance()
Stat2_plot2

data_summary <- temp1 %>%
  distinct(Sample, Status, Group_age) %>% 
  count(Group_age, Status, name = "n_individuals")  # compter les individus uniques

print(data_summary)

