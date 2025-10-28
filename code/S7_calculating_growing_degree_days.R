#Plotting cumulative GDD for all three sites
rm(list=ls())
library(tidyverse)
library(lubridate)

# Load data
url <- "https://raw.githubusercontent.com/RachHThorn/Leaf_traits_phenology/refs/heads/main/data/Hadley_data/Hadley_climate_data_daily_2025.csv"
temps <- read.csv(url)

#Making sure the date is in the correct format 
temps$date <- as.Date(temps$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))

# Defining the base and cutoff temperatures
Tbase <- 4
tasmax_cutoff <- 30
tasmin_cutoff <- Tbase

# Calculating GDD and cumulative GDD for all sites
temps_gdd <- temps %>%
  mutate(
    tasmax_adj = pmin(tasmax, tasmax_cutoff),
    tasmin_adj = pmax(tasmin, tasmin_cutoff),
    Tmean = (tasmax_adj + tasmin_adj) / 2,
    GDD = pmax(0, Tmean - Tbase)
  ) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(cumGDD = cumsum(GDD)) %>%
  ungroup()

#Making sure the date is in the correct format 
temps_gdd$date <- as.Date(temps_gdd$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))

# Exporting site specific GDD results 
temps_gdd %>%
  filter(site == "Wytham") %>%
  write_csv("results/GDD_Wytham_results.csv")

temps_gdd %>%
  filter(site == "Hazelrigg") %>%
  write_csv("results/GDD_Hazelrigg_results.csv")

temps_gdd %>%
  filter(site == "Ainsdale") %>%
  write_csv("results/GDD_Ainsdale_results.csv")


#Plotting the daily GDD for the sites on one plot 

ggplot(temps_gdd, aes(x = date, y = GDD, fill = site)) +
  geom_col(position = "dodge", alpha = 0.7) +
  labs(
    x = "Date",
    y = "Daily GDD (°C·days)",
    fill = "Site"
  ) +
  theme_minimal(base_size = 13)

ggsave("results/S7_daily_gdd_all_sites_plot.png", plot = ggplot, width = 8, height = 6, dpi = 300)


#plotting the cumulative GDD for all three sites on one plot
ggplot(temps_gdd, aes(x = date, y = cumGDD, color = site, group = site)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = "Date",
    y = "Cumulative GDD (°C·days)",
    color = ""
  ) +
  scale_x_date(
    date_breaks = "1 month",       
    date_labels = "%b"            
  ) +
  theme_bw(base_size = 13) +       
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

ggsave("results/S7_cumulative_ GDD_all_sites_plot.png", plot = ggplot, width = 8, height = 6, dpi = 300)
