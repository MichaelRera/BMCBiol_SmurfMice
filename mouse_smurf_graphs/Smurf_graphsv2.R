library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

filename <- file.choose()
filename2 <- str_c(str_sub(filename, 1, -5), "_NS_S.csv" )

loaded_csv <- read_csv(filename,
                       col_types = cols(...1 = col_integer(),
                                        mice = col_integer(), date_expe = col_datetime(format = "%Y-%m-%d"),
                                        GLY = col_integer(), DOB = col_datetime(format = "%Y-%m-%d"),
                                        DOD = col_datetime(format = "%Y-%m-%d"),
                                        AGE = col_integer(), REMAINTIME = col_integer()))
loaded_csv2 <- read_csv(filename2,
                        col_types = cols(...1 = col_integer(),
                                         mice = col_integer(), date_expe = col_date(format = "%Y-%m-%d"),
                                         AGE = col_integer(), PRED = col_character(),
                                         GLY = col_integer()))
merged_tables <- unique(merge(loaded_csv, loaded_csv2, by = c("mice", "date_expe")))

#mice_data <- merged_tables[,c(1,34:35, 66)]
mice_data <- merged_tables[,c("mice","AGE.x", "REMAINTIME.x", "STATUS.y")]
colnames(mice_data) <- c("mice", "AGE", "REMAINTIME", "Smurf_state")

# Transforming the data
df <- mice_data %>%
  group_by(mice) %>%
  mutate(First_S = ifelse(Smurf_state == "S" & !duplicated(Smurf_state == "S"), REMAINTIME, NA)) %>%
  fill(First_S, .direction = "downup") %>%
  ungroup() %>%
  mutate(realigne_Time = REMAINTIME - First_S) %>%
  select(-First_S) %>% 
  mutate(realigne_Time = coalesce(realigne_Time,REMAINTIME))  
# Create a new entry with realigne_Time = REMAINTIME + realigne_Time for each mice
additional_rows <- df %>%
  group_by(mice) %>%
  slice(1) %>% # Get the first entry for each mouse
  mutate(
    realigne_Time = -(AGE - REMAINTIME),
    #realigne_Time = REMAINTIME + realigne_Time,
    AGE = 0, # Adjust AGE if necessary (optional)
    REMAINTIME = realigne_Time,  # Set REMAINTIME to NA for the new row (or leave as is)
    Smurf_state = "birth"  # Set Smurf_state to NA for the new row
  ) %>%
  ungroup() 
# Combine the new entries with the original dataframe
df <- bind_rows(additional_rows, df) %>%
  arrange(mice, realigne_Time) # Sort the data by mice and realigne_Time

# Add new row after the last entry for each mouse
additional_rows_end <- df %>%
  group_by(mice) %>%
  slice_tail(n = 1) %>%  # Get the last entry for each mouse
  mutate(
    AGE = AGE + 1,  # Optional: Adjust AGE if needed
    realigne_Time = realigne_Time - REMAINTIME,  # Calculate the new realigne_Time
    REMAINTIME = 0,  # Set REMAINTIME to NA for the new row
    Smurf_state = "death"  # Set Smurf_state to "death"
  ) %>%
  ungroup()

# Combine the new entries with the original dataframe
df <- df %>% 
  bind_rows(additional_rows_end) %>%
  arrange(mice, realigne_Time)  # Sort the data by mice and realigne_Time 
  

# Calculate the time between 0 and death for each mouse
df <- df %>%
  group_by(mice) %>%
  mutate(death_time = ifelse(Smurf_state == "death", realigne_Time, NA)) %>%
  fill(death_time, .direction = "down") %>%
  mutate(time_0_to_death = ifelse(Smurf_state == "death", -(death_time - 0), NA)) %>%
  ungroup()

df <- df %>%
  mutate(realigne_Time = ifelse(realigne_Time == -1, 0, realigne_Time))

# Determine the order based on the time between 0 and death
mouse_order <- df %>%
  filter(Smurf_state == "death") %>%
  arrange(time_0_to_death) %>%
  pull(mice)

# Define the outline thickness
outline_thickness <- 0.5

# Create the plot
ggplot(df, aes(x = realigne_Time, y = as.numeric(factor(mice, levels = mouse_order)))) +
  geom_rect(data = subset(df, Smurf_state == "birth"), 
            aes(xmin = realigne_Time, xmax = 0, 
                ymin = as.numeric(factor(mice, levels = mouse_order)) - 0.4, 
                ymax = as.numeric(factor(mice, levels = mouse_order)) + 0.4),
            fill = "lightgrey", color = "black", size = outline_thickness) +
  geom_rect(data = subset(df, Smurf_state == "death"),
            aes(xmin = 0, xmax = realigne_Time, 
                ymin = as.numeric(factor(mice, levels = mouse_order)) - 0.4, 
                ymax = as.numeric(factor(mice, levels = mouse_order)) + 0.4),
            fill = "deepskyblue3", color = "black", size = outline_thickness) +
  
  # Adjust vertical lines to match the width of the rectangles minus the outline thickness
  geom_segment(data = subset(df, Smurf_state %in% c("NS", "S")),
               aes(x = realigne_Time, xend = realigne_Time, 
                   y = as.numeric(factor(mice, levels = mouse_order)) - 0.4 + outline_thickness*0.01,
                   yend = as.numeric(factor(mice, levels = mouse_order)) + 0.4 - outline_thickness*0.01,
                   color = Smurf_state),
               linetype = "dashed", size = 1) +
  scale_y_continuous(name = "Mouse", breaks = seq_along(mouse_order), labels = mouse_order) +
  scale_x_continuous(name = "Realigned Time (days)", breaks = c(0,seq(floor(min(df$realigne_Time)/10)*10, ceiling(max(df$realigne_Time)/10)*10, by = 100))) +
  scale_color_manual(values = c("NS" = "darkgrey", "S" = "darkblue")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Timeline of Mice with Birth and Death Periods",
       subtitle = "Grey rectangles represent the period as non-Smurf; Blue rectangles represent the period as Smurf\nDashed lines indicate measurements leading to scoring light grey for 'NS' and light blue for 'S'")

