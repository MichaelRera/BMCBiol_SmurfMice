library(readr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(magrittr)
library(dplyr)
library(ggpubr)
library(gtsummary)
library(scales)
library(tidyr)

#breakpoint calculation
C57_bp <- read.csv("BMC/me_used_and_bp.csv")
C57_bp <- C57_bp %>% transform(me_used_c57 = as.character(me_used_c57))
C57_bp <- C57_bp %>% dplyr::select(variable,me_used_c57,c57_no_me,c57_me)
C57_bp <- C57_bp %>% dplyr::mutate(BP = case_match(me_used_c57, c("FALSE","conv_error") ~ c57_no_me, "TRUE" ~ c57_me)) %>% select(variable,BP)
selection <- c("BW", "BT", "GLY", "P5")
temp_bp <- C57_bp %>% filter(variable %in% selection)
temp_bp <- temp_bp %>%  mutate(BP = ifelse(variable == "BW", -18.71, BP)) %>% 
  mutate(BP = ifelse(variable == "GLY", -29.7, BP)) %>% 
  mutate(BP = ifelse(variable == "FAT", -61.17, BP)) 
  #mutate(BP = ifelse(variable == "WIDs", -36.97, BP)) %>% 
  #mutate(BP = ifelse(variable == "EETOTs", -15.1, BP))
temp_bp <- temp_bp %>% transform(BP = as.numeric(BP))
bp <- mean(temp_bp$BP)
bp

#removal of mice that did not die naturally
C57_M_data <- read_csv("BMC/C57_M_data.csv")
C57_M_data <- C57_M_data %>% 
  dplyr::filter(mice != "36") %>% 
  dplyr::filter(mice != "37") %>% 
  dplyr::filter(mice != "38") %>% 
  dplyr::filter(mice != "39")

#fusion C57_M and C57_F
C57_M_data <- C57_M_data %>% mutate(SEXE = "M") %>% unite(col = "ind", c("mice", "SEXE"), sep = "_")
C57_F_data <- read_csv("BMC/C57_F_data.csv")
C57_F_data <- C57_F_data %>% mutate(SEXE = "F") %>% unite(col = "ind", c("mice", "SEXE"), sep = "_")
C57_data <- bind_rows(C57_F_data, C57_M_data)

#add age in months
C57_data <- C57_data %>% mutate(AGE_MONTHS = AGE/30.5)
C57_data$AGE_MONTHS <- style_number(C57_data$AGE_MONTHS, digits = 1)

#breakpoint-based animal classification
C57_data <- C57_data %>% mutate(REMAINTIME = abs(REMAINTIME)) %>% mutate(PRED = ifelse(REMAINTIME>30,"under30","above30"))

#to take into account only measurement times including all variables
C57_data_lessNA <- C57_data %>% filter(!is.na(RERNm))

#select variables for PCA
temp1a <- C57_data %>% dplyr::select(ind, AGE, AGE_MONTHS, REMAINTIME, PRED, BW, BT, GLY, P5)

#impute missing values to run PCA
temp1b <- temp1a %>% dplyr::select(!c(ind, AGE, AGE_MONTHS, REMAINTIME,PRED))
nb1 <- estim_ncpPCA(temp1b,ncp.max=4, scale = FALSE)
nb1
imputed_temp1a <- imputePCA(temp1a, ncp = 3, scale = TRUE, quali.sup = c(1, 2, 3, 4, 5))

##run and plot PCA
res.pca1 = PCA(imputed_temp1a$completeObs, scale.unit= TRUE, ncp=3, graph=T, quali.sup = c(1, 2, 3, 4, 5))
plot.PCA(res.pca1, axes=c(1, 2), choix="ind", habillage=5, col.hab = "blue", label = "none", cex = 1, invisible = "quali", graph.type = "ggplot", ggoptions = list(size = 1.5))

#assess the impact of imputed values on the PCA
#res.comp1b = MIPCA(temp1b, ncp = 2, nboot = 1000)
#plot(res.comp1b)

#classification of individuals into Smurfs and Non-Smurfs based on PCA dimension 1 values
pca1 <- get_pca_ind(res.pca1)
temp1a_pca1 <- bind_cols(temp1a,pca1$coord) %>% dplyr::select(!c(Dim.2, Dim.3))
pca1_threshold <- temp1a_pca1 %>% group_by(PRED) %>% dplyr::summarise(mean = mean(Dim.1, na.rm = TRUE), sd= sd(Dim.1, na.rm = TRUE)) %>% 
  filter(PRED == "under30") 
value_pca1_threshold <- pca1_threshold$mean-(pca1_threshold$sd*2)
value_pca1_threshold
temp1a_pca1 <- temp1a_pca1 %>% mutate(STATUS = if_else(Dim.1>value_pca1_threshold,"NS","S"))

pca1_threshold_test <- temp1a_pca1 %>% group_by(PRED) %>% dplyr::summarise(mean = mean(Dim.1, na.rm = TRUE), sd= sd(Dim.1, na.rm = TRUE)) %>% 
  filter(PRED == "above30") 
value_pca1_threshold_test <- pca1_threshold_test$mean+(pca1_threshold_test$sd*2)

#figure 4c
temp1a_pca1_fig4c <- temp1a_pca1
temp1a_pca1_fig4c$AGE_MONTHS <- as.factor(temp1a_pca1_fig4c$AGE_MONTHS)
plot_fig4c <- temp1a_pca1_fig4c %>% ggplot(aes(x = AGE_MONTHS, y = REMAINTIME, color = STATUS, shape = STATUS))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  scale_color_manual(values=c("#000000", "#318CE7"))+
  theme_pubclean(base_size = 8)
plot_fig4c

#figure 4c
temp1a_pca1_fig4d <- temp1a_pca1 %>% dplyr::group_by(AGE_MONTHS, STATUS) %>% dplyr::summarise(n = n()) %>% dplyr::mutate(PROP = (n/sum(n))*100) 
plot_fig4d <- temp1a_pca1_fig4d %>% dplyr::filter(STATUS == "S") %>% 
  ggplot(aes(x = AGE_MONTHS, y = PROP))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ exp(x)), linetype = 1)+
  theme_pubclean()
plot_fig4d


