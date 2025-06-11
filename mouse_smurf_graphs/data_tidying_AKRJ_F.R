library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(writexl)

#BT = Body temperature (°C)
BT_AKRJ_F <- read_excel("BMC/AKRJ_F/BT_AKRJ_F.xlsx")
ncol(BT_AKRJ_F)
BT_AKRJ_F <- pivot_longer(BT_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="BT")
BT_AKRJ_F$date_expe <- convert_to_date(BT_AKRJ_F$date_expe)

#BW = Body Weight (g)
BW_AKRJ_F <- read_excel("BMC/AKRJ_F/BW_AKRJ_F.xlsx")
ncol(BW_AKRJ_F)
BW_AKRJ_F <- pivot_longer(BW_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="BW")
BW_AKRJ_F$date_expe <- convert_to_date(BW_AKRJ_F$date_expe)

#EEDm = Average energy expenditure during the day (kcal/h)
EEDm_AKRJ_F <- read_excel("BMC/AKRJ_F/EEDm_AKRJ_F.xlsx")
ncol(EEDm_AKRJ_F)
EEDm_AKRJ_F <- pivot_longer(EEDm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="EEDm")
EEDm_AKRJ_F$date_expe <- convert_to_date(EEDm_AKRJ_F$date_expe)

#EENm = Average energy expenditure during the night (kcal/h)
EENm_AKRJ_F <- read_excel("BMC/AKRJ_F/EENm_AKRJ_F.xlsx")
ncol(EENm_AKRJ_F)
EENm_AKRJ_F <- pivot_longer(EENm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="EENm")
EENm_AKRJ_F$date_expe <- convert_to_date(EENm_AKRJ_F$date_expe)

#EREDm = Average energy expenditure during the night (kcal/h)
EREDm_AKRJ_F <- read_excel("BMC/AKRJ_F/EREDm_AKRJ_F.xlsx")
ncol(EREDm_AKRJ_F)
EREDm_AKRJ_F <- pivot_longer(EREDm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="EREDm")
EREDm_AKRJ_F$date_expe <- convert_to_date(EREDm_AKRJ_F$date_expe)

#FAT = Fat mass (g)
FAT_AKRJ_F <- read_excel("BMC/AKRJ_F/FAT_AKRJ_F.xlsx")
ncol(FAT_AKRJ_F)
FAT_AKRJ_F <- pivot_longer(FAT_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="FAT")
FAT_AKRJ_F$date_expe <- convert_to_date(FAT_AKRJ_F$date_expe)

#FATPROP = FAT proportion of body weight
FATPROP_AKRJ_F <- read_excel("BMC/AKRJ_F/FATPROP_AKRJ_F.xlsx")
ncol(FATPROP_AKRJ_F)
FATPROP_AKRJ_F <- pivot_longer(FATPROP_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="FATPROP")
FATPROP_AKRJ_F$date_expe <- convert_to_date(FATPROP_AKRJ_F$date_expe)

#FIDs = Food intake during the day (g)
FIDs_AKRJ_F <- read_excel("BMC/AKRJ_F/FIDs_AKRJ_F.xlsx")
ncol(FIDs_AKRJ_F)
FIDs_AKRJ_F <- pivot_longer(FIDs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="FIDs")
FIDs_AKRJ_F$date_expe <- convert_to_date(FIDs_AKRJ_F$date_expe)

#FINs = Food intake during the night (g)
FINs_AKRJ_F <- read_excel("BMC/AKRJ_F/FINs_AKRJ_F.xlsx")
ncol(FINs_AKRJ_F)
FINs_AKRJ_F <- pivot_longer(FINs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="FINs")
FINs_AKRJ_F$date_expe <- convert_to_date(FINs_AKRJ_F$date_expe)

#GLY = Glycemia (mg/dl)
GLY_AKRJ_F <- read_excel("BMC/AKRJ_F/GLY_AKRJ_F.xlsx")
ncol(GLY_AKRJ_F)
GLY_AKRJ_F <- pivot_longer(GLY_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="GLY")
GLY_AKRJ_F$date_expe <- convert_to_date(GLY_AKRJ_F$date_expe)

#LEAN = Lean mass (g)
LEAN_AKRJ_F <- read_excel("BMC/AKRJ_F/LEAN_AKRJ_F.xlsx")
ncol(LEAN_AKRJ_F)
LEAN_AKRJ_F <- pivot_longer(LEAN_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="LEAN")
LEAN_AKRJ_F$date_expe <- convert_to_date(LEAN_AKRJ_F$date_expe)

#LEANPROP = LEAN proportion of body weight
LEANPROP_AKRJ_F <- read_excel("BMC/AKRJ_F/LEANPROP_AKRJ_F.xlsx")
ncol(LEANPROP_AKRJ_F)
LEANPROP_AKRJ_F <- pivot_longer(LEANPROP_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="LEANPROP")
LEANPROP_AKRJ_F$date_expe <- convert_to_date(LEANPROP_AKRJ_F$date_expe)

#P0 = Fluorescence in plasma before FITC-Dextran gavage (arbitrary units)
P0_AKRJ_F <- read_excel("BMC/AKRJ_F/P0_AKRJ_F.xlsx")
ncol(P0_AKRJ_F)
P0_AKRJ_F <- pivot_longer(P0_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="P0")
P0_AKRJ_F$date_expe <- convert_to_date(P0_AKRJ_F$date_expe)

#P1 = Fluorescence in plasma 1h after FITC-Dextran gavage (arbitrary units)
P1_AKRJ_F <- read_excel("BMC/AKRJ_F/P1_AKRJ_F.xlsx")
ncol(P1_AKRJ_F)
P1_AKRJ_F <- pivot_longer(P1_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="P1")
P1_AKRJ_F$date_expe <- convert_to_date(P1_AKRJ_F$date_expe)

#P3 = Fluorescence in plasma 3h after FITC-Dextran gavage (arbitrary units)
P3_AKRJ_F <- read_excel("BMC/AKRJ_F/P3_AKRJ_F.xlsx")
ncol(P3_AKRJ_F)
P3_AKRJ_F <- pivot_longer(P3_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="P3")
P3_AKRJ_F$date_expe <- convert_to_date(P3_AKRJ_F$date_expe)

#RERDm = Average respiratory exchange ratio during the day
RERDm_AKRJ_F <- read_excel("BMC/AKRJ_F/RERDm_AKRJ_F.xlsx")
ncol(RERDm_AKRJ_F)
RERDm_AKRJ_F <- pivot_longer(RERDm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="RERDm")
RERDm_AKRJ_F$date_expe <- convert_to_date(RERDm_AKRJ_F$date_expe)

#RERNm = Average respiratory exchange ratio during the night
RERNm_AKRJ_F <- read_excel("BMC/AKRJ_F/RERNm_AKRJ_F.xlsx")
ncol(RERNm_AKRJ_F)
RERNm_AKRJ_F <- pivot_longer(RERNm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="RERNm")
RERNm_AKRJ_F$date_expe <- convert_to_date(RERNm_AKRJ_F$date_expe)

#STATUS = GROUPED vs ISOLATED
STATUS_AKRJ_F <- read_excel("BMC/AKRJ_F/STATUS_AKRJ_F.xlsx")
ncol(STATUS_AKRJ_F)
STATUS_AKRJ_F <- pivot_longer(STATUS_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="STATUS")
STATUS_AKRJ_F$date_expe <- convert_to_date(STATUS_AKRJ_F$date_expe)

#VCO2Dm = CO2 average uptake during the day (ml/h)
VCO2Dm_AKRJ_F <- read_excel("BMC/AKRJ_F/VCO2Dm_AKRJ_F.xlsx")
ncol(VCO2Dm_AKRJ_F)
VCO2Dm_AKRJ_F <- pivot_longer(VCO2Dm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="VCO2Dm")
VCO2Dm_AKRJ_F$date_expe <- convert_to_date(VCO2Dm_AKRJ_F$date_expe)

#VCO2Nm = CO2 average uptake during the night (ml/h)
VCO2Nm_AKRJ_F <- read_excel("BMC/AKRJ_F/VCO2Nm_AKRJ_F.xlsx")
ncol(VCO2Nm_AKRJ_F)
VCO2Nm_AKRJ_F <- pivot_longer(VCO2Nm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="VCO2Nm")
VCO2Nm_AKRJ_F$date_expe <- convert_to_date(VCO2Nm_AKRJ_F$date_expe)

#VO2Dm = O2 average uptake during the day (ml/h)
VO2Dm_AKRJ_F <- read_excel("BMC/AKRJ_F/VO2Dm_AKRJ_F.xlsx")
ncol(VO2Dm_AKRJ_F)
VO2Dm_AKRJ_F <- pivot_longer(VO2Dm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="VO2Dm")
VO2Dm_AKRJ_F$date_expe <- convert_to_date(VO2Dm_AKRJ_F$date_expe)

#VO2Nm = O2 average uptake during the night (ml/h)
VO2Nm_AKRJ_F <- read_excel("BMC/AKRJ_F/VO2Nm_AKRJ_F.xlsx")
ncol(VO2Nm_AKRJ_F)
VO2Nm_AKRJ_F <- pivot_longer(VO2Nm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="VO2Nm")
VO2Nm_AKRJ_F$date_expe <- convert_to_date(VO2Nm_AKRJ_F$date_expe)

#WIDs = Water intake during the day (ml)
WIDs_AKRJ_F <- read_excel("BMC/AKRJ_F/WIDs_AKRJ_F.xlsx")
ncol(WIDs_AKRJ_F)
WIDs_AKRJ_F <- pivot_longer(WIDs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="WIDs")
WIDs_AKRJ_F$date_expe <- convert_to_date(WIDs_AKRJ_F$date_expe)

#WINs = Water intake during the night (ml)
WINs_AKRJ_F <- read_excel("BMC/AKRJ_F/WINs_AKRJ_F.xlsx")
ncol(WINs_AKRJ_F)
WINs_AKRJ_F <- pivot_longer(WINs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="WINs")
WINs_AKRJ_F$date_expe <- convert_to_date(WINs_AKRJ_F$date_expe)

#XYDm = X+Y average activity during the day (counts)
XYDm_AKRJ_F <- read_excel("BMC/AKRJ_F/XYDm_AKRJ_F.xlsx")
ncol(XYDm_AKRJ_F)
XYDm_AKRJ_F <- pivot_longer(XYDm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="XYDm")
XYDm_AKRJ_F$date_expe <- convert_to_date(XYDm_AKRJ_F$date_expe)

#XYNm = X+Y average activity during the night (counts)
XYNm_AKRJ_F <- read_excel("BMC/AKRJ_F/XYNm_AKRJ_F.xlsx")
ncol(XYNm_AKRJ_F)
XYNm_AKRJ_F <- pivot_longer(XYNm_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="XYNm")
XYNm_AKRJ_F$date_expe <- convert_to_date(XYNm_AKRJ_F$date_expe)

#ZDs = Z activity during the day (counts)
ZDs_AKRJ_F <- read_excel("BMC/AKRJ_F/ZDs_AKRJ_F.xlsx")
ncol(ZDs_AKRJ_F)
ZDs_AKRJ_F <- pivot_longer(ZDs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="ZDs")
ZDs_AKRJ_F$date_expe <- convert_to_date(ZDs_AKRJ_F$date_expe)

#ZNs = Z activity during the night (counts)
ZNs_AKRJ_F <- read_excel("BMC/AKRJ_F/ZNs_AKRJ_F.xlsx")
ncol(ZNs_AKRJ_F)
ZNs_AKRJ_F <- pivot_longer(ZNs_AKRJ_F,cols = 2:20, names_to="date_expe", values_to="ZNs")
ZNs_AKRJ_F$date_expe <- convert_to_date(ZNs_AKRJ_F$date_expe)

#Join variables
AKRJ_F_data <- BW_AKRJ_F %>% 
  full_join(BT_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(EEDm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(EENm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(EREDm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(FAT_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(FATPROP_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(FIDs_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(FINs_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(GLY_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(LEAN_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(LEANPROP_AKRJ_F, by = c("mice","date_expe"))%>% 
  full_join(P0_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(P1_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(P3_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(RERDm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(RERNm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(STATUS_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(VCO2Dm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(VCO2Nm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(VO2Dm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(VO2Nm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(WIDs_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(WINs_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(XYDm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(XYNm_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(ZDs_AKRJ_F, by = c("mice","date_expe")) %>% 
  full_join(ZNs_AKRJ_F, by = c("mice","date_expe"))

TIME_AKRJ_F <- read_excel("BMC/AKRJ_F/TIME_AKRJ_F.xlsx")
TIME_AKRJ_F$DOB <- convert_to_date(TIME_AKRJ_F$DOB)
TIME_AKRJ_F$DOD <- convert_to_date(TIME_AKRJ_F$DOD)
AKRJ_F_data <- AKRJ_F_data %>% full_join(TIME_AKRJ_F, by = "mice")

#Create variable AGE = Age of mice and REMAINTIME = Remaining time before death (days)
AKRJ_F_data <- AKRJ_F_data %>% mutate(AGE = date_expe - DOB) %>% 
  mutate(REMAINTIME = date_expe - DOD) %>% 
  mutate(REMAINTIME = ifelse(REMAINTIME>0,-1,REMAINTIME))

#Create new variables
AKRJ_F_data <- AKRJ_F_data %>% 
  mutate(XYTOTs = XYDm*12 + XYNm*12)%>%
  mutate(ZTOTs = ZDs + ZNs) %>%
  mutate(XYZTOTs = XYTOTs + ZTOTs) %>%
  mutate(XYZDs = XYDm*12 + ZDs) %>%
  mutate(XYZNs = XYNm*12 + ZNs) %>%
  mutate(XYDs = XYDm*12) %>%
  mutate(XYNs = XYNm*12) %>%
  mutate(RERTOTm = ((RERDm + RERNm)/2)) %>%
  mutate(EETOTs = EEDm*12 + EENm*12) %>%
  mutate(EEDs = EEDm*12) %>%
  mutate(EENs = EENm*12) %>%
  mutate(FIDKCALs = FIDs*3.339) %>%
  mutate(FINKCALs = FINs*3.339) %>%
  mutate(FITOTKCALs = FIDKCALs + FINKCALs) %>%
  mutate(WITOTs = WIDs + WINs) %>%
  mutate(EB = ((FITOTKCALs)-EETOTs)) %>% 
  mutate (FAOX = EETOTs*((1-RERTOTm)/0.3))

AKRJ_F_data <- AKRJ_F_data %>% subset(!is.na(BW))

#Create excel file containing all dataset AKRJ_F
write.csv(AKRJ_F_data,"BMC/AKRJ_F_data.csv", na = "NA")
