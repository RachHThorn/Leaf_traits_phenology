# R Thornley
# 27/11/2025
# Project: Leaf_traits_phenology
# Script: vegan rough diversity

library(vegan)
library(tidyverse)

################################################################################
# Instructions
################################################################################

# 1) Load in the phenological stage data for all three sites
# 2) transform the data into a matrix with phenological stage as columns and site / time /species 
# combinations as rows
# 3) Apply the vegan diversity functions (there are different ones) - 
# probably simpsons diversity is the best one to use

################################################################################
# code
################################################################################


# read in data
file_path <- ("data/...")
df <- readr::read_csv(file_path, show_col_types = FALSE) 

# transform into a matrix suitable for use in 
wide <- df %>%
  dplyr::select(unique, max_cover, New_taxon) %>%
  pivot_wider(
    names_from = New_taxon,
    values_from = max_cover,
    values_fn = ~ mean(.x, na.rm = TRUE)
  ) %>%
  replace(is.na(.), 0)

spec_mat <- wide %>% dplyr::select(-unique)

# apply the diversity metrics from vegan

diversity <- wide %>%
  transmute(
    unique,
    shannon = vegan::diversity(as.matrix(spec_mat), index = "shannon",  MARGIN = 1),
    simpson = vegan::diversity(as.matrix(spec_mat), index = "simpson",  MARGIN = 1),
    invsimp = vegan::diversity(as.matrix(spec_mat), index = "invsimpson", MARGIN = 1)
  )

################################################################################
# Quickly visualise
################################################################################


# we also need to get variation in the 