
#################################################################################################
#plotting wytham climate data 2000 to current 
#################################################################################################

##################################################################################################
#first attempt at temp graph 
#results in a graph showing the monthly temp up until 2024 then does the 2025 data as daily
#looks very messy and you can't tell the difference clearly in 2025 
#does produce a good plot for the 2000 - 2024 data 

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)

#2000-2024 monthly data 
climate_longterm <- read.csv("data/Hadley_data/Hadley_climate_data_monthly_2000_2024.csv", stringsAsFactors = FALSE)

# Daily climate data for 2025
climate_2025 <- read.csv("data/Hadley_data/Hadley_climate_data_daily_2025.csv", stringsAsFactors = FALSE)

#Date formatting
climate_longterm$date <- as.Date(climate_longterm$date)               
climate_2025$date <- as.Date(climate_2025$date, format = "%d/%m/%Y") 

#Cleaning up site names
climate_longterm$site <- trimws(climate_longterm$site)
climate_2025$site <- trimws(climate_2025$site)

#renaming to Wytham
climate_longterm$site[climate_longterm$site == "Raindrop_Wytham"] <- "Wytham"

#filtering for wytham
longterm_wytham <- climate_longterm %>% filter(site == "Wytham")
climate_2025_wytham <- climate_2025 %>% filter(site == "Wytham")

#preparing 2025 data
climate_2025_wytham <- climate_2025_wytham %>%
  mutate(tasavg = (tasmin + tasmax) / 2)

#calculating the scaling factor for rainfall 
max_rain  <- max(c(longterm_wytham$rainfall, climate_2025_wytham$rainfall), na.rm = TRUE)
temp_range <- max(c(longterm_wytham$tasmax, climate_2025_wytham$tasmax), na.rm = TRUE) -
  min(c(longterm_wytham$tasmin, climate_2025_wytham$tasmin), na.rm = TRUE)
scaleFactor <- max_rain / temp_range

#plotting
ggplot() +
  
  #ribbon for max and min temp (2000–2024)
  geom_ribbon(data = longterm_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "lightgray", alpha = 0.4) +
  
  #monthly mean line
  geom_line(data = longterm_wytham,
            aes(x = date, y = (tasmin + tasmax) / 2),
            color = "black", size = 0.4) +
  
  # 2025 daily rainfall (scaled)
  geom_col(data = climate_2025_wytham,
           aes(x = date, y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.4) +
  
  # 2025 daily temperature 
  geom_ribbon(data = climate_2025_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(data = climate_2025_wytham,
            aes(x = date, y = tasavg),
            color = "red3", size = 0.6) +
  
#Axes and labels 
scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = c(0.01, 0.01)
) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scaleFactor, name = "Rainfall (mm)")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

################################################################################################################
#same graph as above but trying different time-spans and layout to see if it looks better
#changed 2025 to monthly average data 
#ensuring rainfall is correctly plotted 

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)

# 2000–2024 monthly data 
climate_longterm <- read.csv("data/Hadley_data/Hadley_climate_data_monthly_2000_2024.csv", stringsAsFactors = FALSE)

# Daily climate data for 2025
climate_2025 <- read.csv("data/Hadley_data/Hadley_climate_data_daily_2025.csv", stringsAsFactors = FALSE)

# Date formatting
climate_longterm$date <- as.Date(climate_longterm$date)               
climate_2025$date <- as.Date(climate_2025$date, format = "%d/%m/%Y") 

# Clean up site names
climate_longterm$site <- trimws(climate_longterm$site)
climate_2025$site <- trimws(climate_2025$site)

# Rename to Wytham
climate_longterm$site[climate_longterm$site == "Raindrop_Wytham"] <- "Wytham"

# Filter for Wytham
longterm_wytham <- climate_longterm %>% filter(site == "Wytham")
climate_2025_wytham <- climate_2025 %>% filter(site == "Wytham")

# Average daily temperature for 2025
climate_2025_wytham <- climate_2025_wytham %>%
  mutate(tasavg = (tasmin + tasmax) / 2)

# ---- Aggregate 2025 data to monthly ----
climate_2025_monthly <- climate_2025_wytham %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    tasmin = mean(tasmin, na.rm = TRUE),
    tasmax = mean(tasmax, na.rm = TRUE),
    tasavg = mean(tasavg, na.rm = TRUE),
    rainfall = sum(rainfall, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Compute scaling factor dynamically ----
max_rain  <- max(c(longterm_wytham$rainfall, climate_2025_monthly$rainfall), na.rm = TRUE)
temp_range <- max(c(longterm_wytham$tasmax, climate_2025_monthly$tasmax), na.rm = TRUE) -
  min(c(longterm_wytham$tasmin, climate_2025_monthly$tasmin), na.rm = TRUE)
scaleFactor <- max_rain / temp_range

# ---- Plot ----
ggplot() +
  
  # Long-term (2000–2024) temperature range
  geom_ribbon(data = longterm_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "lightgray", alpha = 0.4) +
  
  # Long-term mean temperature
  geom_line(data = longterm_wytham,
            aes(x = date, y = (tasmin + tasmax) / 2),
            color = "black", size = 0.4) +
  
  # Long-term rainfall (scaled)
  geom_col(data = longterm_wytham,
           aes(x = date, y = rainfall / scaleFactor),
           fill = "skyblue3", alpha = 0.3) +
  
  # 2025 monthly rainfall (scaled)
  geom_col(data = climate_2025_monthly,
           aes(x = month, y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.6) +
  
  # 2025 temperature ribbon and mean line
  geom_ribbon(data = climate_2025_monthly,
              aes(x = month, ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(data = climate_2025_monthly,
            aes(x = month, y = tasavg),
            color = "red3", size = 0.7) +
  
  # Axes and labels
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scaleFactor, name = "Rainfall (mm)")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
  )


#########################################################################################################################
#now looking at just 10 years 
#to see if it looks better

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)

# 2000–2024 monthly data 
climate_longterm <- read.csv("data/Hadley_data/Hadley_climate_data_monthly_2000_2024.csv", stringsAsFactors = FALSE)

# Daily climate data for 2025
climate_2025 <- read.csv("data/Hadley_data/Hadley_climate_data_daily_2025.csv", stringsAsFactors = FALSE)

# Date formatting
climate_longterm$date <- as.Date(climate_longterm$date)               
climate_2025$date <- as.Date(climate_2025$date, format = "%d/%m/%Y") 

# Clean up site names
climate_longterm$site <- trimws(climate_longterm$site)
climate_2025$site <- trimws(climate_2025$site)

# Rename to Wytham
climate_longterm$site[climate_longterm$site == "Raindrop_Wytham"] <- "Wytham"

# Filter for Wytham
longterm_wytham <- climate_longterm %>% filter(site == "Wytham")
climate_2025_wytham <- climate_2025 %>% filter(site == "Wytham")

# Average daily temperature for 2025
climate_2025_wytham <- climate_2025_wytham %>%
  mutate(tasavg = (tasmin + tasmax) / 2)

# ---- Aggregate 2025 data to monthly ----
climate_2025_monthly <- climate_2025_wytham %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    tasmin = mean(tasmin, na.rm = TRUE),
    tasmax = mean(tasmax, na.rm = TRUE),
    tasavg = mean(tasavg, na.rm = TRUE),
    rainfall = sum(rainfall, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Filter all data from 2014 onwards ----
longterm_wytham <- longterm_wytham %>% filter(date >= as.Date("2015-01-16"))
climate_2025_monthly <- climate_2025_monthly %>% filter(month >= as.Date("2015-01-16"))

# ---- Compute scaling factor dynamically ----
max_rain  <- max(c(longterm_wytham$rainfall, climate_2025_monthly$rainfall), na.rm = TRUE)
temp_range <- max(c(longterm_wytham$tasmax, climate_2025_monthly$tasmax), na.rm = TRUE) -
  min(c(longterm_wytham$tasmin, climate_2025_monthly$tasmin), na.rm = TRUE)
scaleFactor <- max_rain / temp_range

# ---- Plot ----
ggplot() +
  
  # Long-term (2014–2024) temperature range
  geom_ribbon(data = longterm_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "lightgray", alpha = 0.4) +
  
  # Long-term mean temperature
  geom_line(data = longterm_wytham,
            aes(x = date, y = (tasmin + tasmax) / 2),
            color = "black", size = 0.4) +
  
  # Long-term rainfall (scaled)
  geom_col(data = longterm_wytham,
           aes(x = date, y = rainfall / scaleFactor),
           fill = "skyblue3", alpha = 0.3) +
  
  # 2025 monthly rainfall (scaled)
  geom_col(data = climate_2025_monthly,
           aes(x = month, y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.6) +
  
  # 2025 temperature ribbon and mean line
  geom_ribbon(data = climate_2025_monthly,
              aes(x = month, ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(data = climate_2025_monthly,
            aes(x = month, y = tasavg),
            color = "red3", size = 0.7) +
  
  # Axes and labels
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scaleFactor, name = "Rainfall (mm)")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
  )


###########################################################################################
#second plot (b) focussing in on just 2025 data 

# Get date range from 2025 data
date_min <- min(climate_2025_wytham$date, na.rm = TRUE)
date_max <- max(climate_2025_wytham$date, na.rm = TRUE)

# Compute dynamic rainfall scaling factor from 2025 data only
max_rain  <- max(climate_2025_wytham$rainfall, na.rm = TRUE)
temp_range <- max(climate_2025_wytham$tasmax, na.rm = TRUE) -
  min(climate_2025_wytham$tasmin, na.rm = TRUE)
scaleFactor <- max_rain / temp_range

# Plot 2025 only
ggplot(climate_2025_wytham, aes(x = date)) +
  geom_col(aes(y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.5) +
  geom_ribbon(aes(ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(aes(y = tasavg),
            color = "red3", size = 0.6) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scaleFactor, name = "Rainfall (mm)")
  ) +
  scale_x_date(
    limits = c(date_min, date_max),
    date_breaks = "1 month",
    date_labels = "%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "",
    x = "Month",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black")
  )




