
#################################################################################################
# Plotting Wytham climate data: 2000–2024 monthly + 2025 daily
#################################################################################################

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)

# ---- Load datasets ----

# Monthly climate data for 2000–2024
climate_longterm <- read.csv("data/Hadley_data/Hadley_climate_data_monthly_2000_2024.csv", stringsAsFactors = FALSE)

# Daily climate data for 2025
climate_2025 <- read.csv("data/Hadley_data/Hadley_climate_data_daily_2025.csv", stringsAsFactors = FALSE)

# ---- Date formatting ----
climate_longterm$date <- as.Date(climate_longterm$date)               
climate_2025$date <- as.Date(climate_2025$date, format = "%d/%m/%Y") 

# ---- Clean up site names ----
climate_longterm$site <- trimws(climate_longterm$site)
climate_2025$site <- trimws(climate_2025$site)

#renaming to Wytham
climate_longterm$site[climate_longterm$site == "Raindrop_Wytham"] <- "Wytham"

#Filtering for wytham
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
  
  # Long-term monthly ribbon (2000–2024)
  geom_ribbon(data = longterm_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "lightgray", alpha = 0.4) +
  
  # Long-term monthly mean line
  geom_line(data = longterm_wytham,
            aes(x = date, y = (tasmin + tasmax) / 2),
            color = "black", size = 0.4) +
  
  # 2025 daily rainfall (scaled)
  geom_col(data = climate_2025_wytham,
           aes(x = date, y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.4) +
  
  # 2025 daily temperature ribbon and line
  geom_ribbon(data = climate_2025_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(data = climate_2025_wytham,
            aes(x = date, y = tasavg),
            color = "red3", size = 0.6) +
  
  # ---- Axis and labels ----
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
    axis.title.y.right = element_text(color = "skyblue4"),
    plot.title = element_text(face = "bold")
  )


###########################################################################################
#just looking at 2025 

# Zoom in on 2025 portion only
ggplot() +
  geom_ribbon(data = longterm_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "lightgray", alpha = 0.4) +
  geom_line(data = longterm_wytham,
            aes(x = date, y = (tasmin + tasmax) / 2),
            color = "black", size = 0.4) +
  geom_ribbon(data = climate_2025_wytham,
              aes(x = date, ymin = tasmin, ymax = tasmax),
              fill = "red", alpha = 0.25) +
  geom_line(data = climate_2025_wytham,
            aes(x = date, y = tasavg),
            color = "red3", size = 0.6) +
  geom_col(data = climate_2025_wytham,
           aes(x = date, y = rainfall / scaleFactor),
           fill = "skyblue", alpha = 0.4) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scaleFactor, name = "Rainfall (mm)")
  ) +
  coord_cartesian(xlim = as.Date(c("2025-01-01", "2025-12-31"))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Wytham 2025") +
  theme_classic(base_size = 14)

