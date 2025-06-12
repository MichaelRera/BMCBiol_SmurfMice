library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(rstatix)
library(ggpubr)
library(scales)

sample_cytokines <- read_excel("BMC/sample_cytokines.xlsx")
sample_cytokines <- sample_cytokines %>% 
  dplyr::filter(mice != "3") %>% 
  dplyr::filter(mice != "7") %>% 
  dplyr::filter(mice != "12") %>% 
  dplyr::filter(mice != "13") %>% 
  dplyr::filter(mice != "14")
AKRJ_F_data_NS_S <- read_csv("BMC/AKRJ_F_data_NS_S.csv")

sample_cytokines <- sample_cytokines %>% rename(date_expe = 'date expe')
sample_cytokines <- sample_cytokines %>% unite(mice, date_expe, col = "sample", sep = "_", remove = FALSE)
AKRJ_F_data_NS_S <- AKRJ_F_data_NS_S %>% unite(mice, date_expe, col = "sample", sep = "_", remove = FALSE)
sample_cytokines_NS_S <- inner_join(sample_cytokines, AKRJ_F_data_NS_S, by = "sample")
sample_cytokines_NS_S <- sample_cytokines_NS_S %>% rename(mice = mice.x) %>% rename(date_expe = date_expe.x) %>% select(!c(mice.y, date_expe.y, '...1'))
sample_cytokines_NS_S <- sample_cytokines_NS_S %>% transform(Insulin = as.numeric(Insulin)) %>% transform(Leptin = as.numeric(Leptin))

sample_cytokines_NS_S%>% group_by(STATUS) %>% summarise(n = n())

sample_cytokines_NS_S_long <- sample_cytokines_NS_S %>% select(STATUS, FGF21, `IL-10`, `IL-1β`, `IL-6`, Insulin, Leptin, `MCP-1`, RANTES, `TNF-α`) %>% 
  pivot_longer(-STATUS, names_to = "variables", values_to = "value")
stat.test <- sample_cytokines_NS_S_long %>% group_by(variables) %>% t_test(value ~ STATUS) %>% adjust_pvalue(method = "BH") %>% add_significance()
stat.test

stat1 <- sample_cytokines_NS_S_long %>%
  group_by(variables) %>%
  wilcox_test(value ~ STATUS, paired = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%  
  add_significance()
stat1

sample_cytokines_NS_S_long %>%
  group_by(STATUS, variables) %>%
  summarise(n = n(), .groups = "drop")



#myplot <- ggboxplot(sample_cytokines_NS_S_long, x = "STATUS", y = "value", fill = "STATUS", palette = "npg", legend = "none", ggtheme = theme_pubr(border = TRUE)) + 
  #facet_wrap(~variables, scales = "free_y")
#stat.test <- stat.test %>% add_xy_position(x = "STATUS")
#myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")

p1 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = FGF21, color = STATUS, shape = STATUS))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  scale_color_manual(values=c("#000000", "#318CE7"))+
  guides(color = "none", shape = "none")+
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("FGF21") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p2 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = `IL-10`, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("IL-10") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p3 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = `IL-1β`, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("IL-1β") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p4 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = `IL-6`, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("IL-6") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p5 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = Insulin, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("Insulin") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p6 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = Leptin, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("Leptin") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p7 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = `MCP-1`, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("MCP-1") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p8 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = RANTES, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("RANTES") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

p9 <- sample_cytokines_NS_S %>% ggplot(aes(x = STATUS, y = `TNF-α`, color = STATUS, shape = STATUS)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(position = position_dodge(1)) +
  scale_color_manual(values = c("#000000", "#318CE7")) +
  guides(color = "none", shape = "none") +
  labs(y = "(?)", x = "", color = "", shape = "") + 
  scale_y_continuous(labels = scientific) +
  ggtitle("TNF-α") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = c(0.6, 0.9),  
        legend.text = element_text(size = 10),  
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "grey70"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

plot_grid(p1, p5, p6, p4, p2, p3, p7, p8, p9)

plot_grid(filtered_p1, filtered_p2,filtered_p3, filtered_p4, filtered_p5, filtered_p6, filtered_p7, filtered_p8, filtered_p9)

AKRJ_F_data_NS_S_long <- AKRJ_F_data_NS_S %>% select(STATUS, BW, BT, FAT, FINKCALs, GLY, P1, P3) %>% 
  pivot_longer(-STATUS, names_to = "variables", values_to = "value")
stat.test <- AKRJ_F_data_NS_S_long %>% group_by(variables) %>% t_test(value ~ STATUS) %>% adjust_pvalue(method = "BH") %>% add_significance()
stat.test

AKRJ_F_data_NS_S_long%>% group_by(STATUS) %>% summarise(n = n())

#metabo_myplot <- ggboxplot(AKRJ_F_data_NS_S_long, x = "STATUS", y = "value", fill = "STATUS", palette = "npg", legend = "none", ggtheme = theme_pubr(border = TRUE)) + 
#facet_wrap(~variables, scales = "free_y")
#stat.test <- stat.test %>% add_xy_position(x = "STATUS")
#metabo_myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")

metabo_p1 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = BW))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p2 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = BT))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p3 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = FAT))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p4 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = FINKCALs))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p5 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = GLY))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p6 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = P1))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()
metabo_p7 <- AKRJ_F_data_NS_S %>% ggplot(aes(x = STATUS, y = P3))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  theme_pubclean()

plot_grid(metabo_p1, metabo_p2, metabo_p3, metabo_p4, metabo_p5, metabo_p6, metabo_p7)