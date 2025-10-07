# R Thornley
# 18/09/2025
# Check and tidy raw demo data from 2025 fieldwork - 

library(tidyverse)

################################################################################
# NOTES ON FIELD VARIABLES
################################################################################
# GROWTH

# SIZE
# Width_1 and Width_2 if multiplied together = area (proxy for size)
# If only Width_1 values given and Width_2 is empty - then this value is the area of a mini quadrat
# Height - always given per species 

# NUMBER OF LEAVES
# Only available for some species 

# REPRODUCTIVE OUTPUT

# FLOWER: whether an individual is in flower and what flowering stage / how many flowers
# SEED: whether an individual is seeding and how many seeds

################################################################################
# LOAD / TIDY DATA
################################################################################

demo <- read_csv("data/master_data/demography_final_2025.csv")
names(demo)
head(demo)
glimpse(demo)
demo$Leaves
demo$Site

var_levels <-
  demo %>%
  select(Survival, Seed, Flower) %>% 
  select(where(~ is.factor(.x) || is.character(.x))) %>%
  summarise(across(everything(), ~ list(unique(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "levels") %>%
  unnest(levels)

# create a unique identifier per column
# give the time point column an order

demo <-
  demo %>% 
  dplyr::select(!c(Notes_1, Notes_2)) %>%
  mutate(unique_id = case_when(
    Site == "Wytham" ~ paste0(Site, "_", Treatment, "_", Block, "_", Species, "_", Individual_nos),
    Site == "Hazelrigg" ~ paste0(Site, "_", Treatment, "_", Block, "_", Species, "_", Individual_nos),
    Site == "Ainsdale" ~ paste0(Site, "_", Treatment, "_", Block, "_", Plot_nos, "_", Species, "_", Individual_nos))) %>%
  mutate(Time_point = factor(Time_point, levels = c("T1", "T2", "T3", "T4"))) 


################################################################################
# BINARY / SURVIVAL etc - create data
################################################################################

# create binary value columns for the survival and reproduction columns
# create separate data sets for the survival, reproduction and growth vital rate modelling

survival <- demo %>%
  dplyr::select(unique_id, Site, Treatment, Block, Plot_nos, Individual_nos, Species, 
                Date, Time_point, Survival) %>%
  mutate(Survival_new = case_when(
    Survival == "yes" ~ 1L,
    Survival == "no" ~ 0L,
    is.na(Survival)  ~ NA_integer_,
    TRUE ~ NA_integer_))

flower <- demo %>% 
  filter(!Flower == "Dead") %>% 
  dplyr::select(unique_id, Site, Treatment, Block, Plot_nos, Individual_nos, Species, 
                Date, Time_point, Flower) %>%
  mutate(Flower_new = case_when(
    Flower == "None" ~ 0L,
    is.na(Flower) ~ NA_integer_,
    TRUE ~ 1L))
unique(flower$Flower_new)

seed <- demo %>%
  filter(!Seed == "Dead") %>% 
  dplyr::select(unique_id, Site, Treatment, Block, Plot_nos, Individual_nos, Species, 
                Date, Time_point, Seed) %>%
  mutate(Seed_new = case_when(
    Seed == "None" ~ 0,
    is.na(Seed) ~ NA_integer_,
    TRUE ~ 1L))

################################################################################
# VISUALISE DEMOGRAPHIC CHANGES PER SPECIES OVER TIME WITHIN SPECIES - binary
################################################################################
names(survival)

survival %>% 
  ggplot(aes(Survival_new))+
  geom_histogram()+
  facet_wrap(~Species)
# a survival analysis can only be done for a limited number of species
# most individuals of most species did not die during the sampling period

flower %>% 
  ggplot(aes(Flower_new))+
  geom_histogram()+
  facet_wrap(~Species)

seed %>% 
  ggplot(aes(Seed_new))+
  geom_histogram()+
  facet_wrap(~Species)

################################################################################
# EXPORT THIS DATA FOR GLM modelling
################################################################################

write_csv(survival, "results/survival_binary_evo.csv")
write_csv(flower, "results/flower_binary_evo.csv")
write_csv(seed, "results/seed_binary_evo.csv")

################################################################################
# CREATE CONTINUOUS VARIABLES: SIZE
################################################################################

demo$unique_id
demo$Height
demo$Width_1
demo$Width_2

# Create the area data
demo <- 
  demo %>% 
  mutate(Width_1 = as.numeric(Width_1)) %>%
  mutate(Width_2 = as.numeric(Width_1)) %>%
  mutate(area = case_when(is.numeric(Width_1) & is.numeric(Width_2) ~ Width_1*Width_2,
                                 is.numeric(Width_1) & is.na(Width_2) ~ Width_1,
                                 TRUE ~ NA))
demo$area
demo$Height

################################################################################
# CREATE CONTINUOUS VARIABLES: REPRODUCTION
################################################################################

# These variables have to be sorted per species - some quite detailed information for some species available
demo$Flower
demo$Seed
demo %>% arrange(Species) %>% distinct(Species)

################################################################################
# SPECIES THAT HAVE ALL FLOWERS AT SAME TIME POINT
################################################################################

# AE
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "AE") %>% group_by(Time_point) %>% distinct()
AE <- demo %>%
  filter(Species == "AE") %>%
  mutate(
    nos_flower = case_when(
      Flower == "None" ~ 0,
      Flower == "Dead" ~ NA_real_,
      grepl("^[0-9]+ x (NEB|EB)$", Flower) ~ as.numeric(str_extract(Flower, "^[0-9]+")),
      TRUE ~ NA_real_))
# all the flowers for this species are at T3 - so max number makes the most sense
AE %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()
# there are very few individuals that actually flower during our sampling
AE$nos_flower
AE %>% group_by(unique_id) %>% summarise(flower = sum(nos_flower)) %>% filter(flower >0)
AE <- AE %>% mutate(nos_flower = if_else(nos_flower >= 1, 1L, 0L, missing = NA_integer_))
unique(AE$nos_flower)

# AO
# This species flower continuously across the growing season - take the mean at the individual level
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "AO") %>% group_by(Time_point) %>% distinct()
AO <- demo %>%
  filter(Species == "AO") 
AO %>% ggplot(aes(Time_point, Flower, group = unique_id)) + geom_point()
AO <- AO %>%
  mutate(
    nos_flower = case_when(
      Flower == "None" ~ 0,
      Flower == "Dead" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(Flower))
    )
  ) %>%
  group_by(unique_id) %>%
  mutate(nos_flower = mean(nos_flower, na.rm = TRUE)) %>%
  ungroup()

# BP
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "BP") %>% group_by(Time_point) %>% distinct()
BP <- demo %>%
  filter(Species == "BP") %>%
  mutate(
    nos_flower = case_when(
      Flower == "None" ~ 0,
      Flower == "Dead" ~ NA_real_,
      grepl("^\\d+ x (RS|SH) \\(NIF\\)$", Flower) ~ as.numeric(str_extract(Flower, "^\\d+")),
      TRUE ~ NA_real_))
# all the flowers for this species are at T3 - so max number makes the most sense
BP %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# BS
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "BS") %>% group_by(Time_point) %>% distinct()
BS <- demo %>%
  filter(Species == "BS") %>%
  mutate(
    nos_flower = case_when(
      Flower == "None" ~ 0,
      Flower == "Dead" ~ NA_real_,
      grepl("^\\d+ x RS\\(NIF\\)$", Flower) ~ as.numeric(str_extract(Flower, "^\\d+")),
      TRUE ~ NA_real_))
# all the flowers for this species are at T3 - so max number makes the most sense
BS %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# CV
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "CV") %>% group_by(Time_point) %>% distinct()
CV <- demo %>% 
  filter(Species == "CV") %>%
  mutate(nos_flower = case_when(Species == "CV" & Flower == "None" ~ 0,
                                Species == "CV" & Flower == "Dead" ~ NA,
                                Species == "CV" & Flower == "1 x FS" ~ 1,
                                Species == "CV" & Flower == "2 x FS" ~ 2,
                                Species == "CV" & Flower == "3 x FS" ~ 3,
                                Species == "CV" & Flower == "4 x FS" ~ 4,
                                TRUE ~ NA))
# all the flowers for this species are at T4 - so max number makes the most sense
CV %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# EP
# There are no flowering organs: only cones
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "EP") %>% group_by(Time_point) %>% distinct()
EP<- demo %>% 
  filter(Species == "EP") %>%
  mutate(nos_flower = case_when(Species == "EP" & Flower == "None" ~ 0,
                                TRUE ~ NA))
EP %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# HV
# There are no flowers
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "HV") %>% group_by(Time_point) %>% distinct()
HV <- demo %>% 
  filter(Species == "HV") %>%
  mutate(nos_flower = case_when(Species == "HV" & Flower == "None" ~ 0,
                                TRUE ~ NA))
HV %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# LC
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "LC") %>% group_by(Time_point) %>% distinct()
LC <- demo %>%
  filter(Species == "LC") %>%
  mutate(
    nos_flower = sapply(Flower, function(f) {
      if (is.na(f)) return(NA_real_)
      nums <- str_extract_all(f, "\\d+")[[1]]
      if (length(nums) == 0) return(0)
      sum(as.numeric(nums))
    })
  )
unique(LC$nos_flower)
LC %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()
# now I have to summarise it 
LC <- LC %>% group_by(unique_id) %>%
  mutate(nos_flower = mean(nos_flower, na.rm = TRUE)) %>%
  ungroup()

# MA
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "MA") %>% group_by(Time_point) %>% distinct()
MA <- demo %>%
  filter(Species == "MA") %>%
  mutate(nos_flower = case_when(Species == "MA" & Flower == "None" ~ 0,
                                Species == "MA" & Flower == "1 x NEB" ~ 1,
                                       TRUE ~ NA))
unique(LC$nos_flower)
MA %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()
# check this looks ok - no values are over 1
MA %>% group_by(unique_id) %>% tally(nos_flower) %>% filter(n>0) 

# PD
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "PD") %>% group_by(Time_point) %>% distinct()
PD <- demo %>%
  filter(Species == "PD") %>%
  mutate(nos_flower = case_when(Species == "PD" & Flower == "None" ~ 0,
                                TRUE ~ NA))
unique(PD$nos_flower)
PD %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# RA
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "RA") %>% group_by(Time_point) %>% distinct()
RA <- demo %>%
  filter(Species == "RA") 

# SR
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "SR") %>% group_by(Time_point) %>% distinct()
SR <- demo %>%
  filter(Species == "SR") %>%
  mutate(nos_flower = case_when(Species == "SR" & Flower == "None" ~ 0,
                                TRUE ~ NA))
SR %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()


################################################################################
# BRING THESE DATA TOGETHER

all_simple <- rbind(AE, AO, BP, BS, CV, EP, HV, LC, MA, PD, SR)
names(all_simple)

################################################################################
# SPECIES WITH MORE COMPLEX PHENOLOGY
################################################################################

# PV
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "PV") %>% group_by(Time_point) %>% distinct()
BS <- demo %>%
  filter(Species == "BS") %>%
  mutate(
    nos_flower = case_when(
      Flower == "None" ~ 0,
      Flower == "Dead" ~ NA_real_,
      grepl("^\\d+ x RS\\(NIF\\)$", Flower) ~ as.numeric(str_extract(Flower, "^\\d+")),
      TRUE ~ NA_real_))
# all the flowers for this species are at T3 - so max number makes the most sense
BS %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()


# CA
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "CA") %>% group_by(Time_point) %>% distinct()
CA <- demo %>% 
  filter(Species == "CA") %>%
  mutate(nos_flower = case_when(Species == "CA" & Flower == "None" ~ 0,
                                Species == "CA" & Flower == "None (eaten)" ~ 0,
                                Species == "CA" & Flower == "Dead" ~ NA,
                                Species == "CV" & Flower == "1 x FS" ~ 1,
                                Species == "CV" & Flower == "2 x FS" ~ 2,
                                Species == "CV" & Flower == "3 x FS" ~ 3,
                                Species == "CV" & Flower == "4 x FS" ~ 4,
                                TRUE ~ NA))
# all the flowers for this species are at T4 - so max number makes the most sense
CV %>% ggplot(aes(Time_point, nos_flower, group = unique_id)) + geom_point()

# HN
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "HN") %>% group_by(Time_point) %>% distinct()

# RR
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "RR") %>% group_by(Time_point) %>% distinct()
RR <- demo %>%
  filter(Species == "RR") 

# TA
demo %>% select(Species, Time_point, Flower) %>% filter(Species == "TA") %>% group_by(Time_point) %>% distinct()
