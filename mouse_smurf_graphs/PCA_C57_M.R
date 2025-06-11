library(readr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(magrittr)
library(dplyr)
library(ggpubr)
library(gtsummary)
library(scales)


#breakpoint calculation
C57_M_bp <- read.csv("BMC/me_used_and_bp.csv")
C57_M_bp <- C57_M_bp %>% transform(me_used_c57_m = as.character(me_used_c57_m))
C57_M_bp <- C57_M_bp %>% dplyr::select(variable,me_used_c57_m,c57_m_no_me,c57_m_me)
C57_M_bp <- C57_M_bp %>% dplyr::mutate(BP = case_match(me_used_c57_m, c("FALSE","conv_error") ~ c57_m_no_me, "TRUE" ~ c57_m_me)) %>% select(variable,BP)
selection <- c("BW", "BT", "FAT", "FINKCALs", "GLY", "P5")
temp_bp <- C57_M_bp %>% filter(variable %in% selection)
temp_bp <- temp_bp %>%  mutate(BP = ifelse(variable == "BW", -22.19, BP)) 
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

#breakpoint-based animal classification
C57_M_data <- read_csv("BMC/C57_M_data.csv")
C57_M_data <- C57_M_data %>% mutate(REMAINTIME = abs(REMAINTIME)) %>% mutate(PRED = ifelse(REMAINTIME>31,"under31","above31"))

#to take into account only measurement times including all variables
#C57_M_data_lessNA <- C57_M_data %>% filter(!is.na(RERNm))

#add age in months
C57_M_data <- C57_M_data %>% mutate(AGE_MONTHS = AGE/30.5)
C57_M_data$AGE_MONTHS <- style_number(C57_M_data$AGE_MONTHS, digits = 1)

#select variables for PCA
temp1a <- C57_M_data %>% dplyr::select(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME, PRED, BW, BT, FAT, FINKCALs, GLY, P5)

#impute missing values to run PCA
temp1b <- temp1a %>% dplyr::select(!c(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME,PRED))
nb1 <- estim_ncpPCA(temp1b,ncp.max=6, scale = FALSE)
nb1
imputed_temp1a <- imputePCA(temp1a, ncp = 3, scale = TRUE, quali.sup = c(1, 2, 3, 4, 5, 6))

##run and plot PCA
res.pca1 = PCA(imputed_temp1a$completeObs, scale.unit= TRUE, ncp=3, graph=T, quali.sup = c(1, 2, 3, 4, 5, 6))
plot.PCA(res.pca1, axes=c(1, 2), choix="ind", habillage=6, col.hab = "blue", label = "none", cex = 1, invisible = "quali", graph.type = "ggplot", ggoptions = list(size = 1.5))

#assess the impact of imputed values on the PCA
#res.comp1b = MIPCA(temp1b, ncp = 2, nboot = 1000)
#plot(res.comp1b)

#classification of individuals into Smurfs and Non-Smurfs based on PCA dimension 1 values
pca1 <- get_pca_ind(res.pca1)
temp1a_pca1 <- bind_cols(temp1a,pca1$coord) %>% dplyr::select(!c(Dim.2, Dim.3))
pca1_threshold <- temp1a_pca1 %>% group_by(PRED) %>% dplyr::summarise(mean = mean(Dim.1, na.rm = TRUE), sd= sd(Dim.1, na.rm = TRUE)) %>% 
  filter(PRED == "under31") 
value_pca1_threshold <- pca1_threshold$mean-(pca1_threshold$sd*1.65)
value_pca1_threshold
temp1a_pca1 <- temp1a_pca1 %>% mutate(STATUS = if_else(Dim.1>value_pca1_threshold,"NS","S"))

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

#save table with NS ans S classification
write.csv(temp1a_pca1,"BMC/C57_M_data_NS_S.csv", na = "NA")

