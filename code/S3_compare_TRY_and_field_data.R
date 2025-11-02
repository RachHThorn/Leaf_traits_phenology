# R Thornley
# 16/09/2025
# Look at the distribution of data points from TRY and our data for the key dry leaf traits
rm(list=ls())

library(tidyverse) 

################################################################################
# LOAD THE FIELD DATA
################################################################################

field <- read_csv("results/Field_dry_trait_data_with_flags.csv")

################################################################################
# LOAD TRY DATA AND TIDY 
################################################################################

# load in the TRY trait data
TRY_traits <- read_csv("data/TRY_data/selected_European_data_TRY_our_species.csv")
names(TRY_traits)
# check units
# variable = UnitName
TRY_traits %>% group_by(TraitName) %>% reframe(UnitName) %>% unique()
# LDMC - g/g (in out data we have it as mg/mg but as its a ratio it doens't actually matter)

# make data frame smaller
TRY_traits <- TRY_traits %>% select(AccSpeciesName, New_trait_name, StdValue, UnitName)
# count the number of entries per trait
TRY_traits %>% group_by(New_trait_name) %>% tally()
ggplot(TRY_traits, aes(StdValue)) + geom_density() + facet_wrap(~ New_trait_name, scales = "free")

# filter out for SLA and LDMC
TRY_traits <- TRY_traits %>% filter(New_trait_name %in% c("LDMC", "SLA")) %>% 
  rename(Trait_name = New_trait_name, Trait_value = StdValue)

# Create LMA from SLA: LMA = 1/SLA
TRY_traits
TRY_traits <-
  TRY_traits %>% 
  mutate(Trait_value = if_else(Trait_name == "SLA", 100/Trait_value, Trait_value)) %>%
  mutate(Trait_name = if_else(Trait_name == "SLA", "LMA", Trait_name)) %>%
  mutate(UnitName = if_else(UnitName == "mm2 mg-1", "mg cm-2", UnitName))

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

# visualise the data
TRY_traits %>% 
  ggplot(aes(Trait_value)) + geom_density() + 
  facet_wrap(~ Trait_name, scale = "free_x")

################################################################################
# JOIN THE DATA - COMPARE SPECIES DISTRIBUTIONS
################################################################################

unique(field$is_outlier)
TRY_traits$is_outlier <- FALSE

names(TRY_traits)
names(field)
common_cols <- intersect(names(TRY_traits), names(field))

TRY_traits <- TRY_traits[, common_cols]
field <- field[, common_cols]


both <- rbind(TRY_traits, field)

TRY_traits %>% group_by(Species) %>% tally()

# Define a color palette
my_colors <- c("TRY" = "#00BFC4", "field" = "#F8766D")

#################################################################################################
# L GOWLING
# 02/11/2025
#PLOTTING LMA
###################################################################################################

# LMA
plot_LMA <- both %>%
  filter(Trait_name == "LMA") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(name = "Dataset",                  
                    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),  
                    labels = c("TRY" = "TRY", "field" = "Field")) +
  labs(
    title = "",
    x = "LMA (mg cm⁻²)",
    y = "Density",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14)

plot_LMA

########################################################################################
#ORIGINAL LMA PLOT SKEWED LEFT SO TRYING TWO DIFFERENT APPROACHES
#1. Using coord_cartesin 
#2. log transforming the plot 
###########################################################################################

#1. 
plot_LMA1 <- both %>%
  filter(Trait_name == "LMA") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_fill_manual(
    name = "Dataset",
    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),
    labels = c("TRY" = "TRY", "field" = "Field")
  ) +
  coord_cartesian(xlim = c(0, 100)) +  # adjust to the main LMA range
  labs(
    title = "",
    x = "LMA (mg cm⁻²)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

plot_LMA1

#2. 
plot_LMA2 <- both %>%
  filter(Trait_name == "LMA") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4, adjust = 1.5) +   # smooth the curve
  scale_fill_manual(
    name = "Dataset",
    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),
    labels = c("TRY" = "TRY", "field" = "Field")
  ) +
  scale_x_log10() +  # log-transform x-axis
  labs(
    title = "",
    x = "LMA (mg cm⁻², log scale)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

plot_LMA2

##########################################################################################################
#1.PLOTTING LDMC 
#2. THEN USING COOR_CARTESIAN 
#3.THEN LOG TRANSFORMING
#################################################################################################
#1.
plot_LDMC <- both %>%
  filter(Trait_name == "LDMC") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(name = "Dataset",
                    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),
                    labels = c("TRY" = "TRY", "field" = "Field")) +
  labs(
    title = "LDMC distribution: TRY vs Field",
    x = "LDMC (g/g)",
    y = "Density",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14)

plot_LDMC

#2.
plot_LDMC1 <- both %>%
  filter(Trait_name == "LDMC") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4, adjust = 2) +
  scale_fill_manual(name = "Dataset",                  
                    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),  
                    labels = c("TRY" = "TRY", "field" = "Field")) +
  coord_cartesian(xlim = c(0, 1)) +  
  labs(
    title = "",
    x = "LDMC (g/g)",
    y = "Density",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14)

plot_LDMC1

#3.
plot_LDMC2 <- both %>%
  filter(Trait_name == "LDMC") %>%
  ggplot(aes(x = Trait_value, fill = data_set)) +
  geom_density(alpha = 0.4, adjust = 1.2) +      
  scale_fill_manual(name = "Dataset",
                    values = c("TRY" = "#00BFC4", "field" = "#F8766D"),
                    labels = c("TRY" = "TRY", "field" = "Field")) +
  scale_x_log10() +                             
  labs(
    title = "",
    x = "LDMC (g/g, log scale)",
    y = "Density",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14)

plot_LDMC2

###################################################################################################