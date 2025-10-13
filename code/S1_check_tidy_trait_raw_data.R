# R Thornley
# 16/09/2025
# Check and tidy raw data from 2025 fieldwork - dry leaf traits only
library(tidyverse)

################################################################################
# NOTES ON FIELD VARIABLES
################################################################################

# we already have the following hard traits
# Leaf Area
# Leaf Dry Mass
# Leaf Wet Weight

# create some secondary hard traits from our data
# LMA = Leaf Mass Area = dry weight of a leaf (g) / by the area (cm_2)
# SLA = Specific Leaf Area = area of the leaf (cm_2) / dry weight of a leaf (g)
# LWC - Leaf Water Content = Leaf fresh Mass minus leaf dry mass / fresh mass (can also be x 100)
# LDMC = Leaf Dry Matter Content = oven dry mass of a leaf (g) / fresh mass (g) 
# EWT = Equivalent Water Thickness = Leaf wet weight (g) minus leaf dry weight (g) / leaf area ()

# we also have some 'soft' traits that we recorded as part of the demography
# Plant Height
# Plant width_1 
# Plant width_2 
# Plant size = area = width_1 x width_2
# If none rosette forming species this is an area measurement expressed as a percentage
# these soft traits are probably best examined within a species rather than between

################################################################################
# SOME NOTES ON TRAIT MEASUREMENT UNITS AND CONVERSIONS
################################################################################

# FIELD DATA: 
# leaf fresh weight and dry weight - both measured in g
# leaf area is measured in cm2

# TRY DATA 
# leaf fresh weight and dry weight - both measured in g
# SLA or 1/LMA - mm2 mg-1
# LDMC - g/g

# RTMs (remote sensing)
# LMA (Cdm) = mg cm-2
# EWT = g/m2 (*0.0001 = cm)

# CONVERSIONS
# LDMC: we don't have to convert the field data to compare with TRY data
# LMA / SLA: 

# g - mg  *1000
# cm - mm *10

################################################################################
# SOME NOTES ON EXPECTED VALUES
################################################################################

# For herbs we expect values 
# LMA: TRY: 30-100 and  RTM: 2-10 
# LDMC: 
# EWT: TRY: and RTM: 0.002- 0.02

################################################################################
# EQUATIONS FOR TRAIT CALCULATIONS
################################################################################

# LDMC = dw/fw
# LMA = dw / AL
# EWT = fw-dw/AL
# LWC = (fw - dw) / fw

################################################################################
# READ IN THE FIELD COLLECTED TRAIT DATA AND TIDY VARS
################################################################################

traits <- read_csv("data/master_data/leaf_traits_final_2025.csv")
traits$Leaf_weight_wet_g
traits$Leaf_weight_dry_g
traits$Leaf_area_cm_2
str(traits)
names(traits)

# we have mixed columns here as there are missing values - convert them to numeric
traits <- 
  traits %>%
  mutate(Leaf_area_cm_2 = readr::parse_number(Leaf_area_cm_2), # function parse_number converts to numbers from text strings
         Leaf_weight_dry_g = readr::parse_number(Leaf_weight_dry_g),
         Leaf_weight_wet_g = readr::parse_number(Leaf_weight_wet_g)) %>%
  drop_na(Leaf_area_cm_2, Leaf_weight_dry_g) 

# simplify the data base at this point to just the variables relevant to calculation of dry traits
traits <- traits %>% select(Site, Treatment, Date, Time_point, Species, Individual_nos, Leaf_weight_wet_g,
                  Leaf_area_cm_2, Leaf_weight_dry_g)

# create the secondary traits from the raw data
# LMA = 
# create separate columns for TRY and the RTM variables
traits <-
  traits %>% # drop rows where conversion to numeric failed
  mutate(Leaf_area_mm_2 = Leaf_area_cm_2*10) %>%
  mutate(Leaf_area_m_2 = Leaf_area_cm_2/10000) %>%
  mutate(Leaf_weight_dry_mg = Leaf_weight_dry_g * 1000) %>%
  mutate(Leaf_weight_wet_mg = Leaf_weight_wet_g * 1000) %>%
  mutate(LMA = Leaf_weight_dry_mg / Leaf_area_cm_2) %>%
  mutate(LDMC = Leaf_weight_dry_mg / Leaf_weight_wet_mg) %>%
  mutate(LWC = (Leaf_weight_wet_mg - Leaf_weight_dry_mg) / Leaf_weight_wet_mg) %>% 
  mutate(EWT = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_area_m_2) %>%
  mutate(EWT = EWT * 0.0001)
names(traits)
# EWT - g / m2


################################################################################
# CHECK FOR STRANGE VALUES 
################################################################################

# reshape the data for plotting
traits_long <- 
  traits %>% 
  pivot_longer(cols = c(LMA:EWT), 
               names_to = "Trait_name", values_to = "Trait_value") %>%
  mutate(data_set = "field")

# visualise the data
traits_long %>% 
  ggplot(aes(Trait_value)) + geom_density() + 
  facet_wrap(~ Trait_name, scale = "free_x")

# visualise the data by trait - faceted by species
# keep scales = free so we can see the 
traits_long %>% 
  filter(Trait_name == "EWT") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")

traits_long %>% 
  filter(Trait_name == "LMA") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")

traits_long %>% 
  filter(Trait_name == "LDMC") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")+
  ggtitle("LDMC")
# still some strange values for CV

# See if any of the traits have extreme values
traits_long %>% group_by(Species, Trait_name) %>% summarise(max_value = max(Trait_value))

################################################################################
# FILTER OUT OULIERS # 
################################################################################

# The values are now mostly in the expected ranges but there are still some weird values
# These are probably due to erroneous measurements

# Method 1: uses the IQR to find the outliers and removes them

# find outliers using the is_outlier function
cleaned_data <- traits_long %>%
  group_by(Trait_name, Species) %>% 
  mutate(is_outlier = Trait_value %in% boxplot.stats(Trait_value)$out) %>%
  filter(!is_outlier) %>%   
  ungroup()

# replot to see what this look like
cleaned_data %>% 
  filter(Trait_name == "EWT") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")+
  ggtitle("EWT")

cleaned_data %>% 
  filter(Trait_name == "LMA") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")+
  ggtitle("LMA")

cleaned_data %>% 
  filter(Trait_name == "LDMC") %>% 
  ggplot(aes(Trait_value)) +
  geom_histogram() +
  facet_wrap(~Species, scales = "free_x")+
  ggtitle("LDMC")

# Alternatively we could compare these values with the ones found in TRY

final <- traits_long %>%
  group_by(Trait_name, Species) %>% 
  mutate(is_outlier = Trait_value %in% boxplot.stats(Trait_value)$out) %>%
  ungroup()
final
write_csv(final, "results/Field_dry_trait_data_with_flags.csv")

################################################################################

# Get a data set for use in the variance partitioning models
traits_long <- 
  traits %>% 
  pivot_longer(cols = c(LMA:EWT), 
               names_to = "Trait_name", values_to = "Trait_value") %>%
  mutate(data_set = "field")

cleaned_data <- traits_long %>%
  group_by(Trait_name, Species) %>% 
  mutate(is_outlier = Trait_value %in% boxplot.stats(Trait_value)$out) %>%
  filter(!is_outlier) %>%   
  ungroup() %>%
  dplyr::select(Site, Treatment, Date, Time_point, Species, Individual_nos, 
                Trait_name, Trait_value, is_outlier)

write_csv(cleaned_data, "results/Trait_data_for_var_modelling_evo_demos.csv")



