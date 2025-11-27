# R Thornley
# 16/12/2024

# process all HadUK ceda files to data frame for daily variables
# NOTE: the raw downloaded nc ceda files are large and are stored outside the R project
# produce tidy dataframe with monthly data on the 3 UK DRAGNet sites
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

################################################################################
# MONTHLY HADLEY data for 2000-2023
###############################################################################
###############################################################################
#### WYTHAM #####
################################################################################

# function applied to folder of files with same weather variable
# this data has been downloaded for years 2000-2023 only 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
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
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
# year as a facet 
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# or all data on one plot with year coloured
rainfall %>% 
  ggplot(aes(month, rainfall, colour = year, group = year))+
  geom_point()+
  geom_line()
# this isn't very pretty - need to save years as character / factors

# TASMIN
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmin", 
                               full.names = TRUE)
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Wytham"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  facet_wrap(~year)

netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmax", 
                               full.names = TRUE)
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Wytham"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)

Wytham <- rainfall %>% left_join(tasmax) %>% left_join(tasmin)
Wytham <- Wytham %>% dplyr::select(-c(x, y))

################################################################################
#### AINSDALE #####
################################################################################

# function applied to folder of files with same weather variable
# this data has been downloaded for years 2000-2023 only 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Ainsdale"
# add year and month as separate variables for plotting
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

rainfall <- rainfall %>%
  group_by(date, site) %>%
  mutate(rainfall = mean(rainfall, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, rainfall, site, year, month)

# quick data visualisation
# year as a facet 
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# or all data on one plot with year coloured
rainfall %>% 
  ggplot(aes(month, rainfall, colour = year, group = year))+
  geom_point()+
  geom_line()
# this isn't very pretty - need to save years as character / factors

# TASMIN
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmin", 
                               full.names = TRUE)
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Ainsdale"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

tasmin <- tasmin %>%
  group_by(date, site) %>%
  mutate(tasmin = mean(tasmin, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, tasmin, site, year, month)

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  facet_wrap(~year)

# Load the tasmax data

netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmax", 
                               full.names = TRUE)
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Ainsdale"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))
names(tasmax)

tasmax <- tasmax %>%
  group_by(date, site) %>%
  mutate(tasmax = mean(tasmax, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, tasmax, site, year, month)

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)

# join the three data sets 

Ainsdale <- rainfall %>% left_join(tasmax) %>% left_join(tasmin)
names(Ainsdale)


# quick data visualisation
Ainsdale %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)
Ainsdale %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  facet_wrap(~year)
Ainsdale %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  facet_wrap(~year)

################################################################################
#### HAZELRIGG #####
################################################################################

# function applied to folder of files with same weather variable
# this data has been downloaded for years 2000-2023 only 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/hazelrigg/hazelrigg.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Hazelrigg"
# add year and month as separate variables for plotting
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# get the mean values per date
rainfall <- rainfall %>%
  group_by(date, site) %>%
  mutate(rainfall = mean(rainfall, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, rainfall, site, year, month)

# quick data visualisation
# year as a facet 
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# TASMIN
# folder of files - for min daily air temperature
netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmin", 
                               full.names = TRUE)
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Hazelrigg"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

tasmin <- tasmin %>%
  group_by(date, site) %>%
  mutate(tasmin = mean(tasmin, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, tasmin, site, year, month)

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  facet_wrap(~year)

# Load the tasmax data

netcdf_filename  <- list.files("~/Documents/Hadley_climate_data/Tasmax", 
                               full.names = TRUE)
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Hazelrigg"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))
names(tasmax)

tasmax <- tasmax %>%
  group_by(date, site) %>%
  mutate(tasmax = mean(tasmax, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, tasmax, site, year, month)

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)

# join the three data sets 
Hazelrigg <- rainfall %>% left_join(tasmax) %>% left_join(tasmin)
names(Hazelrigg)


# quick data visualisation
Hazelrigg %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  facet_wrap(~year)
Hazelrigg %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  facet_wrap(~year)
Hazelrigg %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  facet_wrap(~year)

################################################################################
# BIND DATA FOR ALL SITES 2000-2023
################################################################################

# bind sites and variables into one tidy df for the period 2000-2023
all <- rbind(Wytham, Hazelrigg, Ainsdale)
unique(all$site)
all %>% group_by(site) %>% tally()

# export to raw data folder
write_csv(all, "results/Hadley_climate_data_monthly_all_sites_2000_2023.csv")

###########################################################################################
# The 2024 data is in a different format and has to be loaded and tidied separately
###################################################################################

# the 2024 annual data has not been added to the online data base at time of analysis - 
# it was accessed using the following link by request
# https://www.metoffice.gov.uk/hadobs/hadukgrid/
# this requires slightly different code as there is a filef or each month of 2024
# as opposed to one file for the whole year (as code above for year 2000-2023)

################################################################################
# WYTHAM 2024
################################################################################

# folder of files - for rainfall

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Rainfall/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
var_name <- "rainfall"
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
rainfall <- rbindlist(rainfall)

# add site name
rainfall$site <- "Wytham"
# add year as a separate variable
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

rainfall <- rainfall %>%
  group_by(date, site) %>%
  mutate(rainfall = mean(rainfall, na.rm = TRUE)) %>%
  distinct(date, site, .keep_all = TRUE) %>%
  dplyr::select(date, rainfall, site, year, month)

# quick data visualisation
rainfall %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)


# folder of files - for tasmin 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmin/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Wytham"
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

# folder of files - for tasmax
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmax/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/wytham/wytham.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Wytham"
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

Wytham <- rainfall %>% left_join(tasmin) %>% left_join(tasmax) %>% select(-c(x, y))

################################################################################
# AINSDALE 2024
################################################################################

# folder of files - for rainfall

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Rainfall/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
var_name <- "rainfall"
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
rainfall <- rbindlist(rainfall)

# add site name
rainfall$site <- "Ainsdale"
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

rainfall <- rainfall %>%
  group_by(date) %>%
  mutate(rainfall = mean(rainfall, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, rainfall, site, year, month)


# folder of files - for tasmin 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmin/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Ainsdale"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# summarise by means
tasmin <- tasmin %>%
  group_by(date) %>%
  mutate(tasmin = mean(tasmin, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, tasmin, site, year, month)

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

# folder of files - for tasmax
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmax/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/ainsdale/ainsdale.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Ainsdale"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# summarise by means
tasmax <- tasmax %>%
  group_by(date) %>%
  mutate(tasmax = mean(tasmax, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, tasmax, site, year, month)

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

Ainsdale <- rainfall %>% left_join(tasmin) %>% left_join(tasmax)

################################################################################
# HAZELRIGG
################################################################################

# folder of files - for rainfall

netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Rainfall/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/hazelrigg/hazelrigg.shp")
var_name <- "rainfall"
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
rainfall <- rbindlist(rainfall)

# add site name
rainfall$site <- "Hazelrigg"
# add year as a separate variable
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# summarise by means
rainfall <- rainfall %>%
  group_by(date) %>%
  mutate(rainfall = mean(rainfall, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, rainfall, site, year, month)

# quick data visualisation
rainfall %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)


# folder of files - for tasmin 
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmin/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/hazelrigg/hazelrigg.shp")
var_name <- "tasmin"
tasmin <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmin <- rbindlist(tasmin)

# add site name
tasmin$site <- "Hazelrigg"
# add year as a separate variable
tasmin <- tasmin %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# summarise by means
tasmin <- tasmin %>%
  group_by(date) %>%
  mutate(tasmin = mean(tasmin, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, tasmin, site, year, month)

# quick data visualisation
tasmin %>% 
  ggplot(aes(month, tasmin))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

# folder of files - for tasmax
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/HadUK_2024/Tasmax/monthly", full.names = TRUE)
vector_filename <- vect("~/Documents/Hadley_climate_data/Site_polygons/hazelrigg/hazelrigg.shp")
var_name <- "tasmax"
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
tasmax <- rbindlist(tasmax)

# add site name
tasmax$site <- "Hazelrigg"
# add year as a separate variable
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# summarise by means
tasmax <- tasmax %>%
  group_by(date) %>%
  mutate(tasmax = mean(tasmax, na.rm = TRUE)) %>%
  distinct(date, .keep_all = TRUE) %>%
  dplyr::select(date, tasmax, site, year, month)

# quick data visualisation
tasmax %>% 
  ggplot(aes(month, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

Hazelrigg <- rainfall %>% left_join(tasmin) %>% left_join(tasmax)

###############################################################################
# JOIN ALL 2024 ATA
###############################################################################

all_2024 <- 
  Wytham %>% 
  rbind(Ainsdale) %>%
  rbind(Hazelrigg)

# quick data visualisation
all_2024 %>% 
  pivot_longer(cols = c(rainfall, tasmin, tasmax), names_to = "variable", values_to = "values") %>%
  ggplot(aes(month, values))+
  geom_point()+
  geom_line()+
  facet_grid(variable ~ site)

# write_csv(all_2024, "results/all_2024_data.csv")

################################################################################
# JOIN 2000/2023 and 2024 data into one file
################################################################################

dat <- all %>% rbind(all_2024) %>% arrange(year, month)
# quick data visualisation including 2024
dat %>% 
  ggplot(aes(month, rainfall))+
  geom_point()+
  geom_line()+
  facet_grid(site~year)

# export to raw data folder
write_csv(dat, "results/Hadley_climate_data_monthly_all_sites_2000_2024.csv")

################################################################################
# DAILY VALUES CAN ALSO BE CALCULATED
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