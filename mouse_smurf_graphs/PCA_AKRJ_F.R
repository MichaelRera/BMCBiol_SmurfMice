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
AKRJ_F_bp <- read.csv("BMC/me_used_and_bp.csv")
AKRJ_F_bp <- AKRJ_F_bp %>% dplyr::select(variable,me_used_akrj_f,akrj_f_no_me,akrj_f_me)
AKRJ_F_bp <- AKRJ_F_bp %>% dplyr::mutate(BP = case_match(me_used_akrj_f, c("FALSE","conv_error") ~ akrj_f_no_me, "TRUE" ~ akrj_f_me)) %>% select(variable,BP)
selection <- c("BW", "BT", "FAT", "FINKCALs", "GLY", "P1", "P3")
temp_bp <- AKRJ_F_bp %>% filter(variable %in% selection)
temp_bp <- temp_bp %>%  mutate(BP = ifelse(variable == "GLY", -21, BP))
temp_bp <- temp_bp %>% transform(BP = as.numeric(BP))
bp <- mean(temp_bp$BP)
bp

#removal of mice that did not die naturally
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

#to take into account only measurement times including all variables
#AKRJ_F_data_lessNA <- AKRJ_F_data %>% filter(!is.na(RERNm))

#select variables for PCA
temp1a <- AKRJ_F_data %>% dplyr::select(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME, PRED, BW, BT, FAT, FINKCALs, GLY, P1, P3)

#impute missing values to run PCA
temp1b <- temp1a %>% dplyr::select(!c(mice, date_expe, AGE, AGE_MONTHS, REMAINTIME,PRED))
nb1 <- estim_ncpPCA(temp1b,ncp.max=6, scale = FALSE)
nb1
imputed_temp1a <- imputePCA(temp1a, ncp = 4, scale = TRUE, quali.sup = c(1, 2, 3, 4, 5, 6))

#run and plot PCA
res.pca1 = PCA(imputed_temp1a$completeObs, scale.unit= TRUE, ncp=4, graph=T, quali.sup = c(1, 2, 3, 4, 5, 6))
plot.PCA(res.pca1, axes=c(1, 2), choix="ind", habillage=6, col.hab = "blue", label = "none", cex = 1, invisible = "quali", graph.type = "ggplot", ggoptions = list(size = 1.5))

#assess the impact of imputed values on the PCA
#res.comp1b = MIPCA(temp1b, ncp = 4, nboot = 1000)
#plot(res.comp1b)

#classification of individuals into Smurfs and Non-Smurfs based on PCA dimension 1 values
pca1 <- get_pca_ind(res.pca1)
temp1a_pca1 <- bind_cols(temp1a,pca1$coord) %>% dplyr::select(!c(Dim.2, Dim.3, Dim.4))
pca1_threshold <- temp1a_pca1 %>% group_by(PRED) %>% dplyr::summarise(mean = mean(Dim.1, na.rm = TRUE), sd= sd(Dim.1, na.rm = TRUE)) %>% 
  filter(PRED == "under31") 
value_pca1_threshold <- pca1_threshold$mean-(pca1_threshold$sd*1.65)
value_pca1_threshold
temp1a_pca1 <- temp1a_pca1 %>% mutate(STATUS = if_else(Dim.1>value_pca1_threshold,"NS","S"))

#figure 4c
temp1a_pca1_fig4c <- temp1a_pca1
temp1a_pca1_fig4c <- temp1a_pca1_fig4c %>% transform(AGE_MONTHS = as.numeric(AGE_MONTHS))
temp1a_pca1_fig4c$AGE_MONTHS <- as.factor(temp1a_pca1_fig4c$AGE_MONTHS)
plot_fig4c <- temp1a_pca1_fig4c %>% ggplot(aes(x = AGE_MONTHS, y = REMAINTIME, color = STATUS, shape = STATUS))+
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position=position_dodge(1))+
  scale_color_manual(values=c("#000000", "#318CE7"))+
  theme_pubclean()
plot_fig4c

#figure 4d
temp1a_pca1_fig4d <- temp1a_pca1 %>% dplyr::group_by(AGE_MONTHS, STATUS) %>% dplyr::summarise(n = n()) %>% dplyr::mutate(PROP = (n/sum(n))*100) 
temp1a_pca1_fig4d <- temp1a_pca1_fig4d %>% transform(AGE_MONTHS = as.numeric(AGE_MONTHS))
plot_fig4d <- temp1a_pca1_fig4d %>% dplyr::filter(STATUS == "S") %>% 
  ggplot(aes(x = AGE_MONTHS, y = PROP))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ exp(x)), linetype = 1)+
  theme_pubclean()
plot_fig4d

#save table with NS ans S classification
write.csv(temp1a_pca1,"BMC/AKRJ_F_data_NS_S.csv", na = "NA")


