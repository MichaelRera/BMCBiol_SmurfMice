library(readxl)
library(dplyr)
library(tidyverse)

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
ALL_data_NS_S_filtered <- ALL_data_NS_S %>% group_by(mice_name) %>% arrange((date_expe), .by_group = TRUE) %>% 
  filter(any(STATUS == "S") & any(STATUS == "NS")) %>% ungroup()

ALL_data_NS_S_filtered %>% group_by(STATUS) %>% summarise(n = n())
ALL_data_NS_S %>% group_by(STATUS) %>% summarise(n = n())
ALL_data_S_souris_S_only <- ALL_data_NS_S %>%
  group_by(mice_name) %>%
  filter(all(STATUS == "S")) %>%
  ungroup()

temp1 <- ALL_data_NS_S_filtered %>% 
  group_by(mice_name) %>% 
  summarize(date_NS = nth(date_expe[STATUS == "NS"], -2), date_S = first(date_expe[STATUS == "S"])) %>% filter(!is.na(date_NS) & !is.na(date_S))
temp2 <- left_join(temp1, ALL_data_NS_S_filtered, by = c("mice_name" = "mice_name", "date_NS" = "date_expe")) %>% select(-date_S) %>% rename(date_expe = date_NS)
temp3 <- left_join(temp1, ALL_data_NS_S_filtered, by = c("mice_name" = "mice_name", "date_S" = "date_expe")) %>% select(-date_NS) %>% rename(date_expe = date_S)
ALL_data_NS_S_results <-bind_rows(temp2,temp3)

data_cytokines <- read_csv("BMC/sample_cytokines_NS_S.csv")
data_cytokines$mice <- paste0("AKRJ_F_", data_cytokines$mice) 
data_cytokines <- data_cytokines %>% rename(mice_name = mice)
data_cytokines <- data_cytokines %>% select(mice_name, date_expe, sample_name_fanny)
temp4 <- full_join(ALL_data_NS_S_results,data_cytokines, by = c("mice_name" = "mice_name", "date_expe" = "date_expe"))

data_microbiote <- read_csv("BMC/sample_microbiot_NS_S.csv")
data_microbiote$mice <- paste0("AKRJ_F_", data_microbiote$mice) 
data_microbiote <- data_microbiote %>% rename(mice_name = mice)
data_microbiote <- data_microbiote %>% select(mice_name, date_expe, sample_name_vero)
temp5 <- full_join(ALL_data_NS_S_results,data_microbiote, by = c("mice_name" = "mice_name", "date_expe" = "date_expe"))


temp6 <- full_join(temp4,temp5, by = c("mice_name" = "mice_name", "date_expe" = "date_expe"))

ALL_data_NS_S_results_cytokines <- temp4 %>% filter(is.na(sample_name_fanny))
write.csv(ALL_data_NS_S_results_cytokines,"BMC/ALL_data_NS_S_results_cytokines.csv", na = "NA")

ALL_data_NS_S_results_microbiote <- temp5 %>% filter(is.na(sample_name_vero))
write.csv(ALL_data_NS_S_results_microbiote,"BMC/ALL_data_NS_S_results_microbiote.csv", na = "NA")



