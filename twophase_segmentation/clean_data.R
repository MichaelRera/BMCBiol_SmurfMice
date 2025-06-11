rm(list = ls())

# Load libraries --------------------------------------------------
library(dplyr) # Manipulate data frames
library(forcats)
library(tidyr) # for drop_na function
library(ggplot2)
library(dplyr)
library(lubridate) # for handling dates
library(readr); library(readxl) # for intelligent reading of csv and xlsx 
library(stringr)

# data_dir = "whole_nov_2023"
data_dir = "whole_jul_2024"

# c57 males ------
## Load data -----
data_c57_m_raw <- read_csv(file.path("data/raw", data_dir, "C57_M_data.csv"), 
                           col_select = -1, # skip first column
                           col_types = cols(mice = "factor")) 
data_c57_m <- data_c57_m_raw %>% 
  rename(DBD = REMAINTIME) %>% 
  dplyr::select(-date_expe, -DOB, -DOD)

## Set to NAs values of VO2DM and VO2Nm quich are above 1e5
# data_c57_m = data_c57_m %>% 
#   mutate(VO2Dm = ifelse(VO2Dm > 1e5, NA, VO2Dm),
#          VO2Nm = ifelse(VO2Nm > 1e5, NA, VO2Nm))

stopifnot(all(data_c57_m$DBD <= 0)) # check that no data points happened after time of death

## Add information about unnatural death -----
mice_to_remove <- c(36L, 37L, 38L, 39L) # unnatural death

data_c57_m = data_c57_m %>% 
  mutate(unnatural_death = mice %in% mice_to_remove)

## Other outlying values
ggplot(data_c57_m, aes(DBD, BT, color = as.factor(mice))) + 
  geom_point() + geom_line() + ## Clearly two outlying values
  ggtitle("BT")

ggplot(data_c57_m, aes(DBD, VCO2Dm, color = as.factor(mice))) + 
  geom_point() + geom_line() + ## Clearly two outlying values
  ggtitle("VCO2Dm")

ggplot(data_c57_m, aes(DBD, VO2Dm, color = as.factor(mice))) + 
  geom_point() + geom_line() + ## Clearly two outlying values
  ggtitle("VO2Dm")

ggplot(data_c57_m, aes(DBD, VO2Nm, color = as.factor(mice))) + 
  geom_point() + geom_line() + ## Clearly two outlying values
  ggtitle("VO2Nm")

data_c57_m$VO2Dm %>% sort() %>% tail(4)
data_c57_m$VO2Nm %>% sort() %>% tail(4)

## Plot data -----
pivoted <- data_c57_m %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, AGE),
               names_to = "var", values_to = "value")
ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") +
  theme(legend.position = "none") + 
  ggtitle("C57 Males")
ggsave(file = file.path("figure/data_overview", data_dir, "c57_m.pdf"),
       height = 10, width = 16)
## Save data -----
saveRDS(data_c57_m, file = file.path("data/clean", data_dir, "data_c57_m.rds"))


# ----------------
# c57 female -----
# ----------------

## Load data -----
data_c57_f_raw <- read_csv(file.path("data/raw", data_dir, "C57_F_data.csv"), 
                           col_select = -1,  # skip first column
                           col_types = cols(mice = "factor"))
data_c57_f <- data_c57_f_raw %>% 
  rename(DBD = REMAINTIME) %>% 
  dplyr::select(-date_expe, -DOB, -DOD)

stopifnot(all(data_c57_f$DBD <= 0)) # check that no data points happened after time of death

## Plot data -----
pivoted <- data_c57_f %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, AGE),
               names_to = "var",
               values_to = "value")
ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") + scale_x_time() +
  theme(legend.position = "none")
ggsave(file = file.path("figure/data_overview", data_dir, "c57_f.pdf"),
       height = 10, width = 16)

## Save data -----
saveRDS(data_c57_f, file = file.path("data/clean", data_dir, "data_c57_f.rds"))

# -----------------------
# c57 male & female -----
# -----------------------

## Load data -----
data_c57_m <- readRDS(file = file.path("data/clean", data_dir, "data_c57_m.rds")) %>% 
  mutate(sex = "male")
data_c57_f <- readRDS(file = file.path("data/clean", data_dir, "data_c57_f.rds")) %>% 
  mutate(sex = "female") %>% 
  mutate(unnatural_death = FALSE)

## Merge -----
data_c57 = bind_rows(data_c57_m, data_c57_f) %>% 
  mutate(mice = paste0(sex, "_", mice))

## Plot data -----
pivoted <- data_c57 %>% 
  dplyr::select(-AGE) %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, sex),
               names_to = "var",
               values_to = "value")

## select half of the variable names of data_c
var_names1 = names(data_c57)[1:(length(names(data_c57)) %/% 2)]
var_names2 = names(data_c57)[(length(names(data_c57)) %/% 2 + 1):length(names(data_c57))]

ggplot(pivoted %>% filter(var %in% var_names1),
       aes(DBD, value, group = as.factor(mice), color = sex)) +
  geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") + scale_x_time()
ggplot(pivoted %>% filter(var %in% var_names2),
       aes(DBD, value, group = as.factor(mice), color = sex)) +
  geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free_y") + scale_x_time()

## Save data -----
saveRDS(data_c57, file = file.path("data/clean", data_dir, "data_c57.rds"))

# -----------------
# akrj female -----
# -----------------

## Load data -----
data_akrj_f_raw <- read_csv(file.path("data/raw", data_dir, "AKRJ_F_data.csv"), 
                            col_select = -1, # skip first column
                            col_types = cols(mice = "factor")) 
data_akrj_f <- data_akrj_f_raw %>% 
  mutate(date_expe = date_expe %>% parse_date_time(orders = "ymd")) %>% # parse date format
  mutate(DOD = DOD %>% parse_date_time(orders = "ymd")) %>% 
  rename(DBD = REMAINTIME) %>% 
  dplyr::select(-date_expe, -DOB, -DOD)

stopifnot(all(data_akrj_f$DBD <= 0)) # check that no data points happened after time of death

## Add information about unnatural death -----
mice_to_remove <- c(3L, 7L, 12L, 13L, 14L) # unnatural death
data_akrj_f <- data_akrj_f %>% 
  mutate(unnatural_death = mice %in% mice_to_remove)

## Plot data -----
pivoted <- data_akrj_f %>% 
  pivot_longer(cols = !c(mice, DBD, STATUS, AGE),
               names_to = "var",
               values_to = "value")
ggplot(pivoted, aes(DBD, value, color = as.factor(mice))) +
  geom_point(alpha = 0.5, stat = "identity") + geom_line(alpha = 0.5) +
  xlab("Days before death") +
  facet_wrap(~var, scales = "free_y") +
  theme(legend.position = "none") + ggtitle("AKRJ Females")
ggsave(file = file.path("figure/data_overview", data_dir, "akrj_f.pdf"),
       height = 10, width = 16)

## Save data -----
saveRDS(data_akrj_f, file = file.path("data/clean", data_dir, "data_akrj_f.rds"))

