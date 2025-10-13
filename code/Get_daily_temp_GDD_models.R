# R Thornley
# 30/07/2025

# process all HadUK ceda files to data frame for daily variables
# NOTE: the raw downloaded nc ceda files are large and are stored outside the R project
# produce tidy dataframe with monthly data on Wytham Woods Raindrop site
# for 1) rainfall, 2) sunshine hours 3) max and 4) min surface temp

library(tidyverse)
library(terra) # for spatial data analysis
library(data.table)


################################################################################

# function to import folder of files - 
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

#####################################################################################

# Wytham polygon

# read in list of weather nc files downloaded from CEDA
# this data has been downloaded for years 2000-2023 only 
# see code at bottom of this script for the 2024 data
netcdf_filename <- list.files("data/Daily_weather_hadley/rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/Polygons/wytham/wytham.shp")
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

################################################################################

# Ainsdale polygon

# read in list of weather nc files downloaded from CEDA
# this data has been downloaded for years 2000-2023 only 
# see code at bottom of this script for the 2024 data
netcdf_filename <- list.files("data/Daily_weather_hadley/rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/Polygons/ainsdale/ainsdale.shp")
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

################################################################################

# Hazelrigg polygon

# read in list of weather nc files downloaded from CEDA
# this data has been downloaded for years 2000-2023 only 
# see code at bottom of this script for the 2024 data
netcdf_filename <- list.files("data/Daily_weather_hadley/rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/Polygons/Hazelrigg/Hazelrigg.shp")
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

################################################################################

all_rain <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_rain,"results/all_sites_daily_rainfall.csv")

################################################################################
# TMAX
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmin", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/wytham/wytham.shp")
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

netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmin", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/ainsdale/ainsdale.shp")
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

netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmin", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/Hazelrigg/Hazelrigg.shp")
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


################################################################################

all_tasmin <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_tasmin, "results/all_sites_daily_tasmin.csv")

################################################################################

# TMAX
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmax", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/wytham/wytham.shp")
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

netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmax", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/ainsdale/ainsdale.shp")
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

netcdf_filename  <- list.files("data/Daily_weather_hadley/tasmax", 
                               full.names = TRUE)
vector_filename <- vect("data/Polygons/Hazelrigg/Hazelrigg.shp")
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


################################################################################

all_tasmax <- rbind(wytham, ainsdale, hazelrigg)
write_csv(all_tasmax, "results/all_sites_daily_tasmax.csv")

################################################################################




################################################################################
################################################################################

# folder of files - for min daily air temperature
# path to ceda data folder
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmax", 
                               full.names = TRUE)
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Raindrop_Wytham"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)

################################################################################
################################################################################

# folder of files - for monthly sun in years
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Sun", 
                               full.names = TRUE)
var_name <- "sun_hours"
sun <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
sun <- rbindlist(sun)

# add site name
sun$site <- "Raindrop_Wytham"
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

###########################################################################################

# 2024 data #

# the 2024 annual data has not been added to the online data base at time of analysis - 
# it was accessed using the following link by request
# https://www.metoffice.gov.uk/hadobs/hadukgrid/
# this requires slightly different code as there is a filef or each month of 2024
# as opposed to one file for the whole year (as code above for year 2000-2023)

################################################################################

# folder of files - for rainfall

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Rainfall/monthly", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "rainfall"
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
rainfall <- rbindlist(rainfall)

# add site name
rainfall$site <- "Raindrop_Wytham"
# add year as a separate variable
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
rainfall %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

################################################################################

# folder of files - for tasmin 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmin/monthly", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Raindrop_Wytham"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

################################################################################

# folder of files - for tasmax
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmax/monthly", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Raindrop_Wytham"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

################################################################################

# folder of files - for sun
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Sun/monthly", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "sun_hours"
sun <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
sun <- rbindlist(sun)

# add site name
sun$site <- "Raindrop_Wytham"
# add year as a separate variable
sun <- sun %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
sun %>% 
  ggplot(aes(month, sun_hours))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

# bind rainfall and temperature variables into one tidy df for 2024

all_2024 <- 
  rainfall %>% 
  left_join(sun) %>%
  left_join(tasmax) %>%
  left_join(tasmin) %>% 
  select(site, x, y, date, year, month, rainfall, sun_hours, tasmin, tasmax)

# join to other monthly data set

final <-rbind(all_2024, all)
final <- final %>% arrange(year, month)

# quick data visualisation including 2024
final %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

write_csv(final, "results/Hadley_climate_data_monthly_2000_2024.csv")


################################################################################

# daily values are available for 2024 for rainfall, tasmin and tasmax (not sunshine hours)

# RAINFALL

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Rainfall/daily", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "rainfall"
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
rainfall <- rbindlist(rainfall)

# add site name
rainfall$site <- "Raindrop_Wytham"
# add year as a separate variable
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_col()


# TASMIN

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmin/daily", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Raindrop_Wytham"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmin %>% 
  ggplot(aes(date, tasmin))+
  geom_col()

# TASMAX

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmax/daily", full.names = TRUE)
vector_filename <- vect("data/Polygons/Raindrop_experiment_small/Raindrop_experiment_small.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Raindrop_Wytham"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmax %>% 
  ggplot(aes(date, tasmax))+
  geom_col()

# join the daily data for 2024
daily_2024 <- 
  rainfall %>% 
  left_join(tasmax) %>%
  left_join(tasmin) %>% 
  select(site, x, y, date, year, month, rainfall, tasmin, tasmax)

# save the daily data for 2024

write_csv(daily_2024, "results/Hadley_climate_data_daily_2024.csv")