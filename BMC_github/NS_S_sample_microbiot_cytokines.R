library(readxl)
library(dplyr)
library(tidyverse)

sample_microbiot <- read_excel("BMC/sample_microbiot.xlsx")
sample_microbiot <- sample_microbiot %>% 
  dplyr::filter(mice != "3") %>% 
  dplyr::filter(mice != "7") %>% 
  dplyr::filter(mice != "12") %>% 
  dplyr::filter(mice != "13") %>% 
  dplyr::filter(mice != "14")
sample_cytokines <- read_excel("BMC/sample_cytokines.xlsx")
sample_cytokines <- sample_cytokines %>% 
  dplyr::filter(mice != "3") %>% 
  dplyr::filter(mice != "7") %>% 
  dplyr::filter(mice != "12") %>% 
  dplyr::filter(mice != "13") %>% 
  dplyr::filter(mice != "14")
AKRJ_F_data_NS_S <- read_csv("BMC/AKRJ_F_data_NS_S.csv")

sample_microbiot <- sample_microbiot %>% rename(date_expe = 'date expe') %>% rename(sample_name_vero = 'sample')
sample_cytokines <- sample_cytokines %>% rename(date_expe = 'date expe') %>% rename(sample_name_fanny = 'sample')

sample_microbiot <- sample_microbiot %>% unite(mice, date_expe, col = "sample", sep = "_", remove = FALSE)
sample_cytokines <- sample_cytokines %>% unite(mice, date_expe, col = "sample", sep = "_", remove = FALSE)

AKRJ_F_data_NS_S <- AKRJ_F_data_NS_S %>% unite(mice, date_expe, col = "sample", sep = "_", remove = FALSE)

sample_microbiot_NS_S <- inner_join(sample_microbiot, AKRJ_F_data_NS_S, by = "sample")
sample_microbiot_NS_S <- sample_microbiot_NS_S %>% rename(mice = mice.x) %>% rename(date_expe = date_expe.x) %>% select(!c(mice.y, date_expe.y, '...1'))
sample_microbiot_NS_S%>% group_by(STATUS) %>% summarise(n = n())

sample_cytokines_NS_S <- inner_join(sample_cytokines, AKRJ_F_data_NS_S, by = "sample")
sample_cytokines_NS_S <- sample_cytokines_NS_S %>% rename(mice = mice.x) %>% rename(date_expe = date_expe.x) %>% select(!c(mice.y, date_expe.y, '...1'))
sample_cytokines_NS_S%>% group_by(STATUS) %>% summarise(n = n())

write.csv(sample_microbiot_NS_S,"BMC/sample_microbiot_NS_S.csv", na = "NA")
write.csv(sample_cytokines_NS_S,"BMC/sample_cytokines_NS_S.csv", na = "NA")
