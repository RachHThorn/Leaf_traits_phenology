# R Thornley
# 16/09/2025
# Get the leaf traits for our 17 focal species from TRY
# tidy data for comparison with our trait values

library(tidyverse) 
# install.packages("rtry")
library(rtry) # package to load in TRY data easily
library(ggtern) # plotting CSR strategies in ternary plot

################################################################################
# RAW TRY DATA IMPORT #
################################################################################

# Load in the data ordered through TRY
# use the rtry package to load and query data
# data sets are quite large (GBs) so are saved in my documents file not in project folder at the moment

dat_1 <- rtry::rtry_import("data/TRY_data/37407.txt")
dat_2 <- rtry::rtry_import("data/TRY_data/38952.txt")
dat_3 <- rtry::rtry_import("data/TRY_data/43511.txt")
dat <- rbind(dat_1, dat_2, dat_3)
names(dat)


# RAW TRAITS WE ARE INTERESTED IN 
# Leaf dry mass
# Leaf wet mass
# Leaf Area

# DERIVED TRAITS WE ARE INTERESTED IN
# LMA: 
# SLA: given in mm2 mg-1
# LDMC: 

n_distinct(dat$TraitName) # we have 180 traits downloaded from the data base
n_distinct(dat$SpeciesName) # we have these traits for 6760 species

IDs <- unique(dat$TraitID) %>% sort()
IDs

# to get these traits for other species we need the trait ids
# write.csv(t(IDs), "data/TRY_data/Trait_IDs_wanted.csv", row.names = FALSE)

# filter for the traits we want for this analysis
unique(dat$TraitName)
traits <- c("Leaf dry mass (single leaf)", 
            "Leaf fresh mass",
            "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
            "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
            "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)",
            "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
            "Leaf water content per leaf dry mass (not saturated)")

species_wanted <- c("Agrimonia eupatoria", "Brachypodium sylvaticum", 
                    "Brachypodium pinnatum", "Primula veris", 
                    "Clinopodium vulgare", "Hyacinthoides non-scripta", 
                    "Lotus corniculatus", "Taraxacum officinale",
                    "Pulicaria dysenterica", "Hydrocotyle vulgaris", 
                    "Salix repens", "Equisetum palustre", "Mentha aquatica",
                    "Anthoxanthum odoratum", "Ranunculus repens", 
                    "Cirsium arvense", "Rumex acetosa", "HYACINTHOIDES NON-SCRIPTA")   

# see if we have data for all our species
dat %>% 
  filter(SpeciesName %in% species_wanted) %>%
  select(SpeciesName) %>%
  unique()

# we now have traits for all our species

################################################################################
# FILTER THE DATA FOR OUR TARGET SPECIES / VISUALISE
###############################################################################

# to order the data by species ID / name
# we can use the TryAccSpeccies list which is downloadable from the website
species <- read.table("data/TRY_data/TryAccSpecies.txt", sep = "\t", 
                      header = TRUE, fill = TRUE, quote = "")
species$AccSpeciesName
species %>% filter(AccSpeciesName %in% species_wanted)

# plot the data out for one trait to visualise
dat %>% 
  filter(TraitName == "Leaf dry mass (single leaf)") %>%
  filter(SpeciesName %in% species_wanted) %>%
  ggplot(aes(AccSpeciesName, StdValue)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust =  0.5, hjust =1))

# plot the data out for all the key traits to visualise
dat %>% 
  filter(SpeciesName %in% species_wanted) %>%
  filter(TraitName %in% traits) %>% 
  ggplot(aes(AccSpeciesName, StdValue)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust =  0.5, hjust =1))+
  facet_wrap(~ TraitName, scales = "free_y")

# Leaf Fresh Mass - we don't have much data directly for leaf fresh mass
# We need it to calculate EWT which is annoying - it means that we can't look at that directly

# some of these values are on different scales we need to be aware of that
# look at the units
dat %>% 
  filter(SpeciesName %in% species_wanted) %>%
  filter(TraitName %in% traits) %>%
  select(TraitName, UnitName) %>%
  distinct(TraitName, .keep_all = TRUE)

# this needs to be in the same units as our field collected data
unique(dat$TraitName)

# filter the traits and species we want and save the smaller data set for later
small <- 
  dat %>% 
  filter(SpeciesName %in% species_wanted) %>%
  filter(TraitName %in% traits)
write_csv(small, "data/my_species_TRY_data.csv")

################################################################################
# DENSITY PLOTS (Not looking good here!!)
################################################################################

names(small)
# join the two data sets and make a coloured density plot
TRY_traits <- TRY_traits %>% filter(New_trait_name %in% c("LDMC", "SLA")) %>% 
  rename(Trait_name = New_trait_name, Trait_value = StdValue)

small %>% mutate(Species = case_when(AccSpeciesName == "Agrimonia eupatoria" ~ "AE",
                                            AccSpeciesName == "Anthoxanthum odoratum" ~ "AO",
                                            AccSpeciesName == "Brachypodium pinnatum" ~ "BP",
                                            AccSpeciesName == "Brachypodium sylvaticum" ~ "BS",
                                            AccSpeciesName == "Cirsium arvense" ~ "CA",
                                            AccSpeciesName == "Clinopodium vulgare" ~ "CV",
                                            AccSpeciesName == "Equisetum palustre" ~ "EP",
                                            AccSpeciesName == "Hydrocotyle vulgaris" ~ "HV",
                                            AccSpeciesName == "Lotus corniculatus" ~ "LC",
                                            AccSpeciesName == "Mentha aquatica" ~ "MA",
                                            AccSpeciesName == "Primula veris" ~ "PV",
                                            AccSpeciesName == "Pulicaria dysenterica"  ~ "PD",
                                            AccSpeciesName == "Ranunculus repens" ~ "RR",
                                            AccSpeciesName == "Rumex acetosa"  ~ "RA",
                                            AccSpeciesName == "Salix repens"  ~ "SR",
                                            AccSpeciesName == "Taraxacum campylodes" ~ "TA",
                                            AccSpeciesName == "HYACINTHOIDES NON-SCRIPTA" ~ "HN"))
names(TRY_traits)
TRY_traits$data_set <- "TRY"
TRY_traits <- TRY_traits %>% dplyr::select(Trait_name, Trait_value, Species, data_set)

# visualise the data
TRY_traits %>% 
  ggplot(aes(Trait_value)) + geom_density() + 
  facet_wrap(~ Trait_name, scale = "free_x")


################################################################################
# TRYING TO CLEAN UP TO European / temperate occurances only
################################################################################
library(rnaturalearth)
library(rnaturalearthdata)

# have a look at whether we can get enough trait data for UK / northern Europe only
clim <- read.delim("~/Documents/TRY_leaf_trait_data/Try2025130112751TRY_6_Site_Climate_Soil/TRY.WorldClim.out.2022.Nov.2.txt")
names(clim)
# site locations by x and y
clim$LON_site
small_clim <- clim %>% 
  dplyr::select(X, LON_site, LAT_site, observationId)

# clim data the joining variable is specified as follows
# "observationId" 

# TRY trait data for my species joining variable is specified ass follows
# "ObservationID"

# read in small TRY data set
small_try <- read_csv("results/my_species_TRY_data.csv")
small_try <- small_try %>% rename(observationId = ObservationID) 
names(small_try)
new <- small_try %>% left_join(small_clim) # join the two data sets 

# there don't appear to be any geolocated in the clim meta data file
n_distinct(new$LON_site)

# alternative approach - look at the data sets and work out if they are in 
# northern Europe at least
unique(small_try$Dataset) # there are 69 unique data sets
unique(small_try$TraitName) 
see <- small_try %>% group_by(Dataset, TraitName) %>% tally() %>% arrange(-n)
sets_wanted <- c("Jena Experiment Traits", "The LEDA Traitbase", "SwissNationalPark_Engadine",
                 "Div_Resource Pot Experiment", "BIOPOP: Functional Traits For Nature Conservation")
selected_sets <- small_try %>% filter(Dataset %in% sets_wanted)
n_distinct(selected_sets$AccSpeciesName) # we still have 17 species in this filtered data set so might be ok the use

################################################################################

unique(selected_sets$TraitName)

# select just the LDMC and the SLA (1/LMA) - merge the two different SLA categories
selected_sets <-
  selected_sets %>% 
  mutate(New_trait_name = 
           case_when(
             TraitName == "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)" ~ "Leaf_area",
             TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" ~ "LDMC",
             TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" ~ "SLA",
             TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" ~ "SLA",
             TraitName == "Leaf dry mass (single leaf)" ~ "Leaf_dry_mass")) %>%
  filter(New_trait_name != "Leaf_area")

unique(selected_sets$New_trait_name)

selected_sets %>%
  ggplot(aes(AccSpeciesName, StdValue)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust =  0.5, hjust =1))+
  facet_wrap(~ New_trait_name, scales = "free_y")

# check number of data points now by species
selected_sets %>% group_by(New_trait_name, AccSpeciesName) %>% 
  tally() %>% arrange(-n) %>%
  ggplot(aes(New_trait_name, n))+
  geom_col()+
  facet_wrap(~ AccSpeciesName)
# this now looks very low for some species

################################################################################

# now export the selected sets data
write_csv(selected_sets, "data/selected_European_data_TRY_our_species.csv")

selected_sets

TRY_traits <- TRY_traits %>% select(AccSpeciesName, New_trait_name, StdValue)
TRY_traits %>% group_by(New_trait_name) %>% tally()
ggplot(TRY_traits, aes(StdValue)) + geom_density() + facet_wrap(~ New_trait_name, scales = "free")

# join the two data sets and make a coloured density plot
TRY_traits <- TRY_traits %>% filter(New_trait_name %in% c("LDMC", "SLA")) %>% 
  rename(Trait_name = New_trait_name, Trait_value = StdValue)
TRY_traits <- 
  TRY_traits %>% mutate(Species = case_when(AccSpeciesName == "Agrimonia eupatoria" ~ "AE",
                                            AccSpeciesName == "Anthoxanthum odoratum" ~ "AO",
                                            AccSpeciesName == "Brachypodium pinnatum" ~ "BP",
                                            AccSpeciesName == "Brachypodium sylvaticum" ~ "BS",
                                            AccSpeciesName == "Cirsium arvense" ~ "CA",
                                            AccSpeciesName == "Clinopodium vulgare" ~ "CV",
                                            AccSpeciesName == "Equisetum palustre" ~ "EP",
                                            AccSpeciesName == "Hydrocotyle vulgaris" ~ "HV",
                                            AccSpeciesName == "Lotus corniculatus" ~ "LC",
                                            AccSpeciesName == "Mentha aquatica" ~ "MA",
                                            AccSpeciesName == "Primula veris" ~ "PV",
                                            AccSpeciesName == "Pulicaria dysenterica"  ~ "PD",
                                            AccSpeciesName == "Ranunculus repens" ~ "RR",
                                            AccSpeciesName == "Rumex acetosa"  ~ "RA",
                                            AccSpeciesName == "Salix repens"  ~ "SR",
                                            AccSpeciesName == "Taraxacum campylodes" ~ "TA",
                                            AccSpeciesName == "HYACINTHOIDES NON-SCRIPTA" ~ "HN"))
names(TRY_traits)
TRY_traits$data_set <- "TRY"
TRY_traits <- TRY_traits %>% dplyr::select(Trait_name, Trait_value, Species, data_set)

