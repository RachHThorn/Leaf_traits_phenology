# Leaf Traits Phenology Project
# Autumn / Winter 2025
# Rachael Thornley and Lucy Gowling


# script 1
# S1_check_tidy_trait_raw_data.R
# This script is loading in the trait data and making the derived dry trait values
# it also gets rid of any strange values that I can't account for (filters them as outliers)
# It produces the file: Field_dry_trait_data_with_flags.csv (in results folder)
# we should use this file going forward for all our modelling
# any required changes to the trait data should be added to this script and a new
# version of the Field_dry_trait_data_with_flags.csv file created
# this file has outliers flagged which need to be filtered out if required

# script 2
# S2_tidy_TRY_data.R
# This imports the large text files from TRY and tidies them for the species and traits
# we are interested in using in our analysis
# NOTE: the large text files from TRY are not currently stored in data
# we shouldn't try to upload large files such as these to Github as it causes Sync problems
# it exports the smaller data set 'my_species_TRY_data.csv" which we can use for modelling
# I tried to do some rough density plots here but wasn't very happy with the results (needs work)
# it also loads the climate / site based data from TRY and I tried to filter this for 
# European sites only - but there is not much location meta data
# I did filter this somewhat and exported "selected_European_data_TRY_our_species.csv"
# LUCY: have a look at this and see what file you think we should use going forward

# script 3
# I tried to compare the TRY and field trait data but it didn't work very well
# this analysis needs work

# script 4
# variance partitioning
# this script is a bit messy and shows how I got the variance decomposition for all 
# species and different traits
# needs looking through and tidying but produces variance decomp. plot



################################################################################

# other processing code

# Get_daily_temp_GDD_models.R
# This code processes the daily netcdf files from the HadUk data base online
# the latest data is not collated and has to be tidied a bit which this code does
# it exports the files into the data folder: Hadley_data - "all_sites_daily_rainfall_2025.csv" (and tasmax tasmin)