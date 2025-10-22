rm(list = ls ())
install.packages(c("ggplot2", "dplyr", "zoo", "lubridate"))
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

#uploading file 
climate_df <- read.csv("C:/Users/lucyg/OneDrive/Documents/NEW PROJECT (BES POSTER + MANUSCRIPT)/Datasets + spreadsheets/All climate data.csv", stringsAsFactors = FALSE)

###########################################################################################################################################################
#Temp and precipitation trends
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

#checking its the right date format
climate_df$date <- as.Date(climate_df$date, format = "%d/%m/%Y")

#cleaning up the site names
climate_df$site <- trimws(climate_df$site)

# summarising temp and rainfall by site and week 
climate_week <- climate_df %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(site, week) %>%
  summarise(
    tasmax = mean(tasmax, na.rm = TRUE),
    tasmin = mean(tasmin, na.rm = TRUE),
    rainfall = mean(rainfall, na.rm = TRUE),
    .groups = "drop"
  )

# making scaling factor for rianfall 
max_rain  <- max(climate_week$rainfall, na.rm = TRUE)
temp_range <- max(climate_week$tasmax, na.rm = TRUE) - min(climate_week$tasmin, na.rm = TRUE)
scaleFactor <- max_rain / temp_range

s
sites <- unique(climate_week$site)


plot_list <- list()
for (s in sites) {
  df_site <- climate_week %>% filter(site == s)
  
  df_site <- df_site %>%
    mutate(tasavg = (tasmin + tasmax) / 2)
  
  p <- ggplot(df_site, aes(x = week)) +
    geom_col(aes(y = rainfall / scaleFactor), fill = "skyblue", alpha = 0.4) +
    geom_ribbon(aes(ymin = tasmin, ymax = tasmax), fill = "gray", alpha = 0.3) +
    geom_line(aes(y = tasavg), color = "gray40", size = 0.5, linetype = "solid") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(
      name = "Temperature (°C)",
      limits = c(-5, 30), 
      breaks = seq(-5, 30, by = 5), 
      sec.axis = sec_axis(~ . * scaleFactor, name = "Average Weekly Rainfall (mm)")
    )+
    labs(
      x = "Date",
      title = paste(s)
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 360, hjust = 0.6),
      axis.title.x = element_text(size = 9),
      axis.title.y.left  = element_text(color = "black", size = 9),
      axis.title.y.right = element_text(color = "black", size = 9),
      plot.title = element_text(size = 11)
    )
  plot_list[[s]] <- p
}

plot_list[[1]]          
plot_list[[2]]         
plot_list[[3]] 