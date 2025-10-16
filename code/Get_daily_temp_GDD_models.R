# R Thornley
# 30/07/2025 / updated 16/10/2025

# process all HadUK ceda files to data frame for daily variables
# NOTE: the raw downloaded nc ceda files are large and are stored outside the R project
# as they are quite large
# produce tidy dataframe with monthly data for teh following sites
# 1) Wytham Woods 
# 2) Ainsdale and sand dunes
# 3) Hazelrigg

# For the following variables
# 1) rainfall
# 2) min surface temp (tasmin)
# 4) max surface temp (tasmax)

library(tidyverse)
library(terra) # for spatial data analysis
library(data.table)

################################################################################
# FUNCTION TO TRANSFORM folder of netcdf files to a csv data file for a specific polygon
################################################################################

# convert nc file to raster and then filter area by site vector
# this function also works with multiple sites if the vector contains multiple labelled polygons
# tidy raster and extract values including x, y, co-ordinates
# requires a vector and netcdf filename as input, plus the climate variable name (eg rainfall)

netcdf_to_df <- function(netcdf_filename, vector_filename, var_name) {
  
  dat <- terra::rast(netcdf_filename)
  time <- terra::time(dat)
  raster_crs <- terra::crs(dat, proj = TRUE)
  sites_new <- terra::project(vector_filename, raster_crs)
  small_raster <- terra::crop(dat, sites_new, snap = "out")
  df <- terra::extract(small_raster, sites_new, snap = "out", xy = TRUE)
  sites <- sites_new$field
  x_coords <- df$x
  y_coords <- df$y
  df$ID <- NULL
  df$x <- NULL
  df$y <- NULL
  names(df) <- time
  df$sites <- sites
  df$x <- x_coords
  df$y <- y_coords
  df <- df %>% pivot_longer(cols = c(-x, -y), names_to = "date", values_to = var_name)
  df$date <- as_date(df$date)
  return(df)
  
}

##############################################################################
# Process the daily rainfall data
###############################################################################

# Wytham polygon

# read in list of weather nc files downloaded from CEDA
# see code at bottom of this script for the 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Wytham"
# add year and month as separate variables for plotting
wytham  <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
# year as a facet 
wytham %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# Ainsdale polygon

# read in list of weather nc files downloaded from CEDA
# this data has been downloaded for years 2000-2023 only 
# see code at bottom of this script for the 2024 data
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
plot(vector_filename)
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Ainsdale"
# add year and month as separate variables for plotting
ainsdale <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
# year as a facet 
ainsdale %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# this needs simplifying down as there are multiple 1km squares covered by our polygon

# Hazelrigg polygon

# read in list of weather nc files downloaded from CEDA
# this data has been downloaded for years 2000-2023 only 
# see code at bottom of this script for the 2024 data
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/Hazelrigg/Hazelrigg.shp")
plot(vector_filename)
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Hazelrigg"
# add year and month as separate variables for plotting
hazelrigg <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
# year as a facet 
hazelrigg %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# this needs simplifying down as there are multiple 1km squares covered by our polygon

# save csv

all_rain <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_rain,"data/Hadley_data/all_sites_daily_rainfall_2025.csv")

################################################################################
# Process the daily tasmin data
################################################################################

# Wytham
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmin", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Wytham"
# add year as a separate variable
wytham <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
wytham %>% 
  ggplot(aes(date, tasmin))+
  geom_point()

# Ainsdale
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmin", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Ainsdale"
# add year as a separate variable
ainsdale <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
ainsdale %>% 
  ggplot(aes(date, tasmin))+
  geom_point()

# Hazelrigg
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmin", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/Hazelrigg/Hazelrigg.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Hazelrigg"
# add year as a separate variable
hazelrigg <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
hazelrigg %>% 
  ggplot(aes(date, tasmin))+
  geom_point()

all_tasmin <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_tasmin, "data/Hadley_data//all_sites_daily_tasmin_2025.csv")

################################################################################
# Process the daily tasmax data
################################################################################

# folder of files - for min daily air temperature
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmax", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Wytham"
# add year as a separate variable
wytham <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
wytham %>% 
  ggplot(aes(date, tasmax))+
  geom_point()

netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmax", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Ainsdale"
# add year as a separate variable
ainsdale <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
ainsdale %>% 
  ggplot(aes(date, tasmax))+
  geom_point()

netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Tasmax", 
                               full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/Hazelrigg/Hazelrigg.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Hazelrigg"
# add year as a separate variable
hazelrigg <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
hazelrigg %>% 
  ggplot(aes(date, tasmax))+
  geom_point()

all_tasmax <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_tasmax, "data/Hadley_data/all_sites_daily_tasmax_2025.csv")

################################################################################
# Process the monthly sun data
################################################################################
# folder of files - for monthly sun in years
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/HadUK_2025/Sun", 
                               full.names = TRUE)
var_name <- "sun_hours"
sun <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
sun <- rbindlist(sun)

# add site name
sun$site <- "Wytham"
# add year as a separate variable
sun <- sun %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
sun %>% 
  ggplot(aes(month, sun_hours))+
  geom_point()+
  facet_wrap(~year)



################################################################################

# bind rainfall and temperature variables into one tidy df for the period 2000-2023

all <- 
  rainfall %>% 
  left_join(sun) %>%
  left_join(tasmax) %>%
  left_join(tasmin)

# export to raw data folder
# write_csv(all, "results/Hadley_climate_data_monthly.csv")
