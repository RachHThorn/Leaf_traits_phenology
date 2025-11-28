#######################################################################################################################################################################################################
#CODE FOR PLOTTING PHENOLOGICAL STAGES OF SPECIES 
#All plots facet wrapped
##############################################################################################################
rm(list = ls())
library(readr)
library(ggplot2)
library(dplyr)
library(ggh4x)


data <- read.csv("data/master_data/demography_final_2025_with_phenology_column.csv", stringsAsFactors = FALSE)

#Species into full species names 
species_lookup <- c(
  "AE" = "Agrimonia eupatoria",    
  "AO" = "Anthoxanthum odoratum",     
  "BP" = "Brachypodium pinnatum",
  "BS" = "Brachypodium sylvaticum",
  "CA" = "Cirsium arvense",
  "CV" = "Clinopodium vulgare", 
  "EP" = "Equistetum palustre", 
  "HN" = "Hyacinthoides nonscripta",
  "HV" = "Hydrocotyle vulgaris", 
  "LC" = "Lotus corniculatus", 
  "MA" = "Mentha aquatica", 
  "PD" = "Pulicaria dysenterica", 
  "PV" = "Primula veris",
  "RA" = "Rumex acetosa",
  "RR" = "Ranunculus repens",
  "SR" = "Salix repens",
  "TA" = "Taraxacum officinale agg."
)

data$Species <- species_lookup[data$Species]

#Converting into phenological codes
data <- data %>%
  mutate(`Phenological.stage` = case_when(
    `Phenological.stage` == 1 ~ "Flowering",
    `Phenological.stage` == 0 ~ "Non-Reproductive",
    `Phenological.stage` == 2 ~ "Seeding",
    `Phenological.stage` == 3 ~ "Flowering and Seeding",
    `Phenological.stage` == 4 ~ "Dead",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(`Phenological.stage`))

#converting time to a factor
data$Time_point <- factor(data$Time_point)

#ensuring months are in the cirrect order 
data$Time_point <- factor(data$Time_point, levels = c("April", "May", "June", "July"))

#plotting
ggplot(data, aes(x = Time_point, fill = `Phenological.stage`)) +
  geom_bar(position = "fill") +
  facet_wrap2(~Species, ncol = 4, strip = strip_vanilla(), axes = "x")+
  labs(y = "Proportion", x = NULL, fill = "Phenological Stage") +
  scale_fill_manual(values = c(
    "Non-Reproductive" = "lightblue",
    "Flowering" = "hotpink1",
    "Dead" = "grey",
    "Seeding" = "orange",
    "Flowering and Seeding" = "violet"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    axis.text.y = element_text(size = 5),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.key.size = unit(0.2, "cm"),
    strip.text = element_text(size = 3, face = "bold")
  )

#####################################################################################################################################
#CODE FOR PLOTTING INDIVIDUAL PLOTS OF EACH SPECIES PHENOLOGICAL STAGES
##########################################################################################################################

rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)
library(ggh4x)

data <- read.csv("data/master_data/demography_final_2025_with_phenology_column.csv", stringsAsFactors = FALSE)

#Species into full species names 
species_lookup <- c(
  "AE" = "Agrimonia eupatoria",    
  "AO" = "Anthoxanthum odoratum",      
  "BP" = "Brachypodium pinnatum",
  "BS" = "Brachypodium sylvaticum",
  "CA" = "Cirsium arvense",
  "CV" = "Clinopodium vulgare", 
  "EP" = "Equisetum palustre", 
  "HN" = "Hyacinthoides non-scripta",
  "HV" = "Hydrocotyle vulgaris", 
  "LC" = "Lotus corniculatus", 
  "MA" = "Mentha aquatica", 
  "PD" = "Pulicaria dysenterica", 
  "PV" = "Primula veris",
  "RA" = "Rumex acetosa",
  "RR" = "Ranunculus repens",
  "SR" = "Salix repens",
  "TA" = "Taraxacum officinale"
)

data$Species <- species_lookup[data$Species]

#converting phenological codes to labels 
data <- data %>%
  mutate(`Phenological.stage` = case_when(
    `Phenological.stage` == 1 ~ "Flowering",
    `Phenological.stage` == 0 ~ "Non-Reproductive",
    `Phenological.stage` == 2 ~ "Seeding",
    `Phenological.stage` == 3 ~ "Flowering and Seeding",
    `Phenological.stage` == 4 ~ "Dead",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(`Phenological.stage`))

#Ensuring months are ordered correctly
data <- data %>%
  mutate(Time_point = trimws(Time_point),
         Time_point = factor(Time_point, levels = c("March", "April", "May", "June", "July", "August", "September")),
         Time_num = as.numeric(Time_point))

#plotting one species at a time 
species_list <- unique(data$Species)

for (sp in species_list) {
  
  sp_data <- data %>% filter(Species == sp)
  
  if (nrow(sp_data) == 0) {
    message("Skipping ", sp, " — no data to plot.")
    next
  }
  
  p <- ggplot(sp_data, aes(x = Time_num, fill = `Phenological.stage`)) +
    geom_bar(position = "fill", width = 0.6) +
    scale_x_continuous(breaks = 1:7,
                       labels = levels(data$Time_point),
                       limits = c(1, 7)) +
    labs(title = sp,
         y = "Proportion", x = NULL,
         fill = "Phenological Stage") +
    scale_fill_manual(values = c(
      "Non-Reproductive" = "lightblue",
      "Flowering" = "hotpink1",
      "Dead" = "grey",
      "Seeding" = "orange",
      "Flowering and Seeding" = "violet"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.3, "cm"),
      plot.title = element_text(size = 8, face = "bold")
    )
  
  print(p)
}
