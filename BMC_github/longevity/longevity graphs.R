library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(writexl)

#AKRJ_F
df <- read_excel("BMC/longevity/LONGEVITY_AKRJ_F.xlsx")
df <- df %>% filter(!mice %in% c(3, 7, 12, 13, 14))
df$DOB <- as.Date(df$DOB)
df$DOD <- as.Date(df$DOD)
df$lifespan <- as.numeric(df$DOD - df$DOB)
max_survivor <- df %>% filter(lifespan == max(lifespan)) %>% slice(1)
reference_DOB <- max_survivor$DOB
status_data <- df %>% select(-DOB, -DOD, -lifespan)
long_data <- status_data %>%
  pivot_longer(cols = -mice, names_to = "date", values_to = "status")
long_data$date <- convert_to_date(long_data$date)
long_data$age_days <- as.numeric(long_data$date - reference_DOB) / 30.44

summary_data <- long_data %>%
  group_by(age_days, date) %>%
  summarise(percent_alive = mean(status == "ALIVE") * 100, .groups = "drop") %>%
  mutate(label = format(date, "%m-%d-%Y"))

summary_data <- summary_data %>%
  add_row(age_days = 0, percent_alive = 100, date = as.Date(NA), label = NA, .before = 1)

ggplot(summary_data, aes(x = age_days, y = percent_alive)) +
  
  geom_segment(aes(x = age_days, xend = age_days, y = 0, yend = percent_alive),
               linetype = "dotted", color = "gray40") +
  geom_line(color = "black") +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, color = "grey30", size = 0.8) +
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  labs(
    title = "AKR/J females",
    x = "Age since birth (months)",
    y = "mice alive (%)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    panel.grid = element_blank(),
    axis.line = element_blank(),    
    axis.ticks = element_blank()    
  )

#C57_M
df1 <- read_excel("BMC/longevity/LONGEVITY_C57_M.xlsx")
df1 <- df1 %>% filter(!mice %in% c(36, 37, 38, 39))
df1$DOB <- as.Date(df1$DOB)
df1$DOD <- as.Date(df1$DOD)
df1$lifespan <- as.numeric(df1$DOD - df1$DOB)
max_survivor <- df1 %>% filter(lifespan == max(lifespan)) %>% slice(1)
reference_DOB <- max_survivor$DOB
status_data <- df1 %>% select(-DOB, -DOD, -lifespan)
long_data <- status_data %>%
  pivot_longer(cols = -mice, names_to = "date", values_to = "status")
long_data$date <- convert_to_date(long_data$date)
long_data$age_days <- as.numeric(long_data$date - reference_DOB) / 30.44

summary_data <- long_data %>%
  group_by(age_days, date) %>%
  summarise(percent_alive = mean(status == "ALIVE") * 100, .groups = "drop") %>%
  mutate(label = format(date, "%m-%d-%Y"))

summary_data <- summary_data %>%
  add_row(age_days = 22, percent_alive = 100, date = as.Date(NA), label = NA, .before = 1)

ggplot(summary_data, aes(x = age_days, y = percent_alive)) +
  
  geom_segment(aes(x = age_days, xend = age_days, y = 0, yend = percent_alive),
               linetype = "dotted", color = "gray40") +
  geom_line(color = "black") +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, color = "grey30", size = 0.8) +
  geom_vline(xintercept = 22, color = "grey30", size = 0.8) +
  labs(
    title = "C57BL6/J males",
    x = "Age since birth (months)",
    y = "mice alive (%)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    panel.grid = element_blank(),
    axis.line = element_blank(),    
    axis.ticks = element_blank()    
  )

#C57_F
df2 <- read_excel("BMC/longevity/LONGEVITY_C57_F.xlsx")
df2$DOB <- as.Date(df2$DOB)
df2$DOD <- as.Date(df2$DOD)
df2$lifespan <- as.numeric(df2$DOD - df2$DOB)
max_survivor <- df2 %>% filter(lifespan == max(lifespan)) %>% slice(1)
reference_DOB <- max_survivor$DOB
status_data <- df2 %>% select(-DOB, -DOD, -lifespan)
long_data <- status_data %>%
  pivot_longer(cols = -mice, names_to = "date", values_to = "status")
long_data$date <- convert_to_date(long_data$date)
long_data$age_days <- as.numeric(long_data$date - reference_DOB) / 30.44

summary_data <- long_data %>%
  group_by(age_days, date) %>%
  summarise(percent_alive = mean(status == "ALIVE") * 100, .groups = "drop") %>%
  mutate(label = format(date, "%m-%d-%Y"))

summary_data <- summary_data %>%
  add_row(age_days = 18, percent_alive = 100, date = as.Date(NA), label = NA, .before = 1)

ggplot(summary_data, aes(x = age_days, y = percent_alive)) +
  
  geom_segment(aes(x = age_days, xend = age_days, y = 0, yend = percent_alive),
               linetype = "dotted", color = "gray40") +
  geom_line(color = "black") +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, color = "grey30", size = 0.8) +
  geom_vline(xintercept = 18, color = "grey30", size = 0.8) +
  labs(
    title = "C57BL6/J females",
    x = "Age since birth (months)",
    y = "mice alive (%)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    panel.grid = element_blank(),
    axis.line = element_blank(),    
    axis.ticks = element_blank()    
  )