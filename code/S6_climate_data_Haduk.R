# R Thornley
# 16/10/2025
# Load and update the climate data from the Haduk
# incorporate the more up to date data we can download from 
# the online repository 
# https://www.metoffice.gov.uk/hadobs/hadukgrid/

library(tidyverse)

################################################################################
# check the data we have at the moment
################################################################################

dat <- read_csv("data/Hadley_data/Hadley_climate_data_monthly_2000_2024.csv")
names(dat)
dat$year
unique(dat$date)
# we don't have any 2025 climate data here

names(dat)

################################################################################
# Load new data for the available months for 2025
# this has been processed by the script 'Get_daily_temp_GDD_models.R'
################################################################################
rainfall_2025 <- read_csv("data/Hadley_data/all_sites_daily_rainfall_2025.csv")
tasmax_2025 <- read_csv("data/Hadley_data/all_sites_daily_tasmax_2025.csv")
tasmin_2025 <- read_csv("data/Hadley_data/all_sites_daily_tasmin_2025.csv")

names(rainfall_2025)

all_2025 <- rainfall_2025 %>% left_join(tasmin_2025) %>% left_join(tasmax_2025)

###############################################################################

all <- dat %>% bind_rows(all_2025)
unique(all$year)
