print("This file was created within RStudio")

print("And now it lives on GitHub")

#Plotting GDD for all three sites
# Combined GDD calculation and plotting for Wytham and Hazelrigg

library(tidyverse)
library(lubridate)

# Load data
temps <- read.csv("C:/Users/lucyg/OneDrive/Documents/NEW PROJECT (BES POSTER + MANUSCRIPT)/Datasets + spreadsheets/All climate data.csv")

# Checking date format
temps$date <- as.Date(temps$date, format = "%d/%m/%Y")

# Defining base and cutoff temperatures
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

# Exporting site specific GDD results 
temps_gdd %>%
  filter(site == "Wytham") %>%
  write.csv("C:/Users/lucyg/OneDrive/Documents/NEW PROJECT (BES POSTER + MANUSCRIPT)/Datasets + spreadsheets/GDD_Wytham_results.csv", row.names = FALSE)

temps_gdd %>%
  filter(site == "Hazelrigg") %>%
  write.csv("C:/Users/lucyg/OneDrive/Documents/NEW PROJECT (BES POSTER + MANUSCRIPT)/Datasets + spreadsheets/GDD_Hazelrigg_results.csv", row.names = FALSE)

# Plotting
# 1. Daily GDD for both sites

ggplot(temps_gdd, aes(x = date, y = GDD, fill = site)) +
  geom_col(position = "dodge", alpha = 0.7) +
  labs(
    x = "Date",
    y = "Daily GDD (°C·days)",
    fill = "Site"
  ) +
  theme_minimal(base_size = 13)

# 2. Cumulative GDD for both sites on one graph
ggplot(temps_gdd, aes(x = date, y = cumGDD, color = site)) +
  geom_line(linewidth = 0.7) +
  labs(
    x = "Date",
    y = "Cumulative GDD (°C·days)",
    color = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top"
  )
