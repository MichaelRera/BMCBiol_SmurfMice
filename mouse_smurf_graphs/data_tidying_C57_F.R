library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(writexl)

#BT = Body temperature (°C)
BT_C57_F <- read_excel("BMC/C57_F/BT_C57_F.xlsx")
ncol(BT_C57_F)
BT_C57_F <- pivot_longer(BT_C57_F,cols = 2:23, names_to="date_expe", values_to="BT")
BT_C57_F$date_expe <- convert_to_date(BT_C57_F$date_expe)

#BW = Body Weight (g)
BW_C57_F <- read_excel("BMC/C57_F/BW_C57_F.xlsx")
ncol(BW_C57_F)
BW_C57_F <- pivot_longer(BW_C57_F,cols = 2:23, names_to="date_expe", values_to="BW")
BW_C57_F$date_expe <- convert_to_date(BW_C57_F$date_expe)

#EEDm = Average energy expenditure during the day (kcal/h)
EEDm_C57_F <- read_excel("BMC/C57_F/EEDm_C57_F.xlsx")
ncol(EEDm_C57_F)
EEDm_C57_F <- pivot_longer(EEDm_C57_F,cols = 2:23, names_to="date_expe", values_to="EEDm")
EEDm_C57_F$date_expe <- convert_to_date(EEDm_C57_F$date_expe)

#EENm = Average energy expenditure during the night (kcal/h)
EENm_C57_F <- read_excel("BMC/C57_F/EENm_C57_F.xlsx")
ncol(EENm_C57_F)
EENm_C57_F <- pivot_longer(EENm_C57_F,cols = 2:23, names_to="date_expe", values_to="EENm")
EENm_C57_F$date_expe <- convert_to_date(EENm_C57_F$date_expe)

#EREDm = Average energy expenditure during the night (kcal/h)
EREDm_C57_F <- read_excel("BMC/C57_F/EREDm_C57_F.xlsx")
ncol(EREDm_C57_F)
EREDm_C57_F <- pivot_longer(EREDm_C57_F,cols = 2:23, names_to="date_expe", values_to="EREDm")
EREDm_C57_F$date_expe <- convert_to_date(EREDm_C57_F$date_expe)

#FAT = Fat mass (g)
FAT_C57_F <- read_excel("BMC/C57_F/FAT_C57_F.xlsx")
ncol(FAT_C57_F)
FAT_C57_F <- pivot_longer(FAT_C57_F,cols = 2:23, names_to="date_expe", values_to="FAT")
FAT_C57_F$date_expe <- convert_to_date(FAT_C57_F$date_expe)

#FIDs = Food intake during the day (g)
FIDs_C57_F <- read_excel("BMC/C57_F/FIDs_C57_F.xlsx")
ncol(FIDs_C57_F)
FIDs_C57_F <- pivot_longer(FIDs_C57_F,cols = 2:23, names_to="date_expe", values_to="FIDs")
FIDs_C57_F$date_expe <- convert_to_date(FIDs_C57_F$date_expe)

#FINs = Food intake during the night (g)
FINs_C57_F <- read_excel("BMC/C57_F/FINs_C57_F.xlsx")
ncol(FINs_C57_F)
FINs_C57_F <- pivot_longer(FINs_C57_F,cols = 2:23, names_to="date_expe", values_to="FINs")
FINs_C57_F$date_expe <- convert_to_date(FINs_C57_F$date_expe)

#GLY = Glycemia (mg/dl)
GLY_C57_F <- read_excel("BMC/C57_F/GLY_C57_F.xlsx")
ncol(GLY_C57_F)
GLY_C57_F <- pivot_longer(GLY_C57_F,cols = 2:23, names_to="date_expe", values_to="GLY")
GLY_C57_F$date_expe <- convert_to_date(GLY_C57_F$date_expe)

#LEAN = Lean mass (g)
LEAN_C57_F <- read_excel("BMC/C57_F/LEAN_C57_F.xlsx")
ncol(LEAN_C57_F)
LEAN_C57_F <- pivot_longer(LEAN_C57_F,cols = 2:23, names_to="date_expe", values_to="LEAN")
LEAN_C57_F$date_expe <- convert_to_date(LEAN_C57_F$date_expe)

#P0 = Fluorescence in plasma before FITC-Dextran gavage (arbitrary units)
P0_C57_F <- read_excel("BMC/C57_F/P0_C57_F.xlsx")
ncol(P0_C57_F)
P0_C57_F <- pivot_longer(P0_C57_F,cols = 2:23, names_to="date_expe", values_to="P0")
P0_C57_F$date_expe <- convert_to_date(P0_C57_F$date_expe)

#P1 = Fluorescence in plasma 1h after FITC-Dextran gavage (arbitrary units)
P1_C57_F <- read_excel("BMC/C57_F/P1_C57_F.xlsx")
ncol(P1_C57_F)
P1_C57_F <- pivot_longer(P1_C57_F,cols = 2:23, names_to="date_expe", values_to="P1")
P1_C57_F$date_expe <- convert_to_date(P1_C57_F$date_expe)

#P3 = Fluorescence in plasma 3h after FITC-Dextran gavage (arbitrary units)
P3_C57_F <- read_excel("BMC/C57_F/P3_C57_F.xlsx")
ncol(P3_C57_F)
P3_C57_F <- pivot_longer(P3_C57_F,cols = 2:23, names_to="date_expe", values_to="P3")
P3_C57_F$date_expe <- convert_to_date(P3_C57_F$date_expe)

#P5 = Fluorescence in plasma 5h after FITC-Dextran gavage (arbitrary units)
P5_C57_F <- read_excel("BMC/C57_F/P5_C57_F.xlsx")
ncol(P5_C57_F)
P5_C57_F <- pivot_longer(P5_C57_F,cols = 2:23, names_to="date_expe", values_to="P5")
P5_C57_F$date_expe <- convert_to_date(P5_C57_F$date_expe)

#RERDm = Average respiratory exchange ratio during the day
RERDm_C57_F <- read_excel("BMC/C57_F/RERDm_C57_F.xlsx")
ncol(RERDm_C57_F)
RERDm_C57_F <- pivot_longer(RERDm_C57_F,cols = 2:23, names_to="date_expe", values_to="RERDm")
RERDm_C57_F$date_expe <- convert_to_date(RERDm_C57_F$date_expe)

#RERNm = Average respiratory exchange ratio during the night
RERNm_C57_F <- read_excel("BMC/C57_F/RERNm_C57_F.xlsx")
ncol(RERNm_C57_F)
RERNm_C57_F <- pivot_longer(RERNm_C57_F,cols = 2:23, names_to="date_expe", values_to="RERNm")
RERNm_C57_F$date_expe <- convert_to_date(RERNm_C57_F$date_expe)

#STATUS = GROUPED vs ISOLATED
STATUS_C57_F <- read_excel("BMC/C57_F/STATUS_C57_F.xlsx")
ncol(STATUS_C57_F)
STATUS_C57_F <- pivot_longer(STATUS_C57_F,cols = 2:23, names_to="date_expe", values_to="STATUS")
STATUS_C57_F$date_expe <- convert_to_date(STATUS_C57_F$date_expe)

#VCO2Dm = CO2 average uptake during the day (ml/h)
VCO2Dm_C57_F <- read_excel("BMC/C57_F/VCO2Dm_C57_F.xlsx")
ncol(VCO2Dm_C57_F)
VCO2Dm_C57_F <- pivot_longer(VCO2Dm_C57_F,cols = 2:23, names_to="date_expe", values_to="VCO2Dm")
VCO2Dm_C57_F$date_expe <- convert_to_date(VCO2Dm_C57_F$date_expe)

#VCO2Nm = CO2 average uptake during the night (ml/h)
VCO2Nm_C57_F <- read_excel("BMC/C57_F/VCO2Nm_C57_F.xlsx")
ncol(VCO2Nm_C57_F)
VCO2Nm_C57_F <- pivot_longer(VCO2Nm_C57_F,cols = 2:23, names_to="date_expe", values_to="VCO2Nm")
VCO2Nm_C57_F$date_expe <- convert_to_date(VCO2Nm_C57_F$date_expe)

#VO2Dm = O2 average uptake during the day (ml/h)
VO2Dm_C57_F <- read_excel("BMC/C57_F/VO2Dm_C57_F.xlsx")
ncol(VO2Dm_C57_F)
VO2Dm_C57_F <- pivot_longer(VO2Dm_C57_F,cols = 2:23, names_to="date_expe", values_to="VO2Dm")
VO2Dm_C57_F$date_expe <- convert_to_date(VO2Dm_C57_F$date_expe)

#VO2Nm = O2 average uptake during the night (ml/h)
VO2Nm_C57_F <- read_excel("BMC/C57_F/VO2Nm_C57_F.xlsx")
ncol(VO2Nm_C57_F)
VO2Nm_C57_F <- pivot_longer(VO2Nm_C57_F,cols = 2:23, names_to="date_expe", values_to="VO2Nm")
VO2Nm_C57_F$date_expe <- convert_to_date(VO2Nm_C57_F$date_expe)

#WIDs = Water intake during the day (ml)
WIDs_C57_F <- read_excel("BMC/C57_F/WIDs_C57_F.xlsx")
ncol(WIDs_C57_F)
WIDs_C57_F <- pivot_longer(WIDs_C57_F,cols = 2:23, names_to="date_expe", values_to="WIDs")
WIDs_C57_F$date_expe <- convert_to_date(WIDs_C57_F$date_expe)

#WINs = Water intake during the night (ml)
WINs_C57_F <- read_excel("BMC/C57_F/WINs_C57_F.xlsx")
ncol(WINs_C57_F)
WINs_C57_F <- pivot_longer(WINs_C57_F,cols = 2:23, names_to="date_expe", values_to="WINs")
WINs_C57_F$date_expe <- convert_to_date(WINs_C57_F$date_expe)

#XYDm = X+Y average activity during the day (counts)
XYDm_C57_F <- read_excel("BMC/C57_F/XYDm_C57_F.xlsx")
ncol(XYDm_C57_F)
XYDm_C57_F <- pivot_longer(XYDm_C57_F,cols = 2:23, names_to="date_expe", values_to="XYDm")
XYDm_C57_F$date_expe <- convert_to_date(XYDm_C57_F$date_expe)

#XYNm = X+Y average activity during the night (counts)
XYNm_C57_F <- read_excel("BMC/C57_F/XYNm_C57_F.xlsx")
ncol(XYNm_C57_F)
XYNm_C57_F <- pivot_longer(XYNm_C57_F,cols = 2:23, names_to="date_expe", values_to="XYNm")
XYNm_C57_F$date_expe <- convert_to_date(XYNm_C57_F$date_expe)

#ZDs = Z activity during the day (counts)
ZDs_C57_F <- read_excel("BMC/C57_F/ZDs_C57_F.xlsx")
ncol(ZDs_C57_F)
ZDs_C57_F <- pivot_longer(ZDs_C57_F,cols = 2:23, names_to="date_expe", values_to="ZDs")
ZDs_C57_F$date_expe <- convert_to_date(ZDs_C57_F$date_expe)

#ZNs = Z activity during the night (counts)
ZNs_C57_F <- read_excel("BMC/C57_F/ZNs_C57_F.xlsx")
ncol(ZNs_C57_F)
ZNs_C57_F <- pivot_longer(ZNs_C57_F,cols = 2:23, names_to="date_expe", values_to="ZNs")
ZNs_C57_F$date_expe <- convert_to_date(ZNs_C57_F$date_expe)

#Join variables
C57_F_data <- BW_C57_F %>% 
  full_join(BT_C57_F, by = c("mice","date_expe")) %>% 
  full_join(EEDm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(EENm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(EREDm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(FAT_C57_F, by = c("mice","date_expe")) %>% 
  full_join(FIDs_C57_F, by = c("mice","date_expe")) %>% 
  full_join(FINs_C57_F, by = c("mice","date_expe")) %>% 
  full_join(GLY_C57_F, by = c("mice","date_expe")) %>% 
  full_join(LEAN_C57_F, by = c("mice","date_expe")) %>% 
  full_join(P0_C57_F, by = c("mice","date_expe")) %>% 
  full_join(P1_C57_F, by = c("mice","date_expe")) %>% 
  full_join(P3_C57_F, by = c("mice","date_expe")) %>%
  full_join(P5_C57_F, by = c("mice","date_expe")) %>% 
  full_join(RERDm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(RERNm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(STATUS_C57_F, by = c("mice","date_expe")) %>% 
  full_join(VCO2Dm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(VCO2Nm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(VO2Dm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(VO2Nm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(WIDs_C57_F, by = c("mice","date_expe")) %>% 
  full_join(WINs_C57_F, by = c("mice","date_expe")) %>% 
  full_join(XYDm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(XYNm_C57_F, by = c("mice","date_expe")) %>% 
  full_join(ZDs_C57_F, by = c("mice","date_expe")) %>% 
  full_join(ZNs_C57_F, by = c("mice","date_expe"))

TIME_C57_F <- read_excel("BMC/C57_F/TIME_C57_F.xlsx")
TIME_C57_F$DOB <- convert_to_date(TIME_C57_F$DOB)
TIME_C57_F$DOD <- convert_to_date(TIME_C57_F$DOD)
C57_F_data <- C57_F_data %>% full_join(TIME_C57_F, by = "mice")

#Create variable AGE = Age of mice and REMAINTIME = Remaining time before death (days)
C57_F_data <- C57_F_data %>% mutate(AGE = date_expe - DOB) %>% 
  mutate(REMAINTIME = date_expe - DOD) %>% 
  mutate(REMAINTIME = ifelse(REMAINTIME>0,-1,REMAINTIME))

#Create new variables
C57_F_data <- C57_F_data %>% 
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
  mutate (FAOX = EETOTs*((1-RERTOTm)/0.3)) %>% 
  mutate(FATPROP = (FAT/BW)*100) %>%
  mutate(LEANPROP = (LEAN/BW)*100)

C57_F_data <- C57_F_data %>% subset(!is.na(BW))
#Create excel file containing all dataset C57_F
write.csv(C57_F_data,"BMC/C57_F_data.csv", na = "NA")