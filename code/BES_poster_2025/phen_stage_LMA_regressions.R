# R Thornley
# 05/12/2025
# Models of correlations between variation in phenological stages and LMA
# at the species/time-point level

library(lme4)
library(tidyverse)
library(purrr)
library(easystats)

################################################################################
# INSTRUCTIONS
###############################################################################


################################################################################
# Load the trait data
###############################################################################

# we want to find out if the variance in LMA within a site and time-point is correlated with variance
# in species phenological stages

# we first need to calculate the variance in each species phenological stages in one time point
# try using simpson's index

# load the trait data
traits <- read_csv("results/Trait_data_for_var_modelling_evo_demos.csv") 
names(traits)

# filter out the outliers which are flagged from the old script
traits <- traits %>% filter(is_outlier == FALSE)
# make the data set small with only the variables and traits we need
names(traits)
traits <- traits %>% 
  dplyr::select(Site, Treatment, Date, Time_point, Species, Trait_name, Trait_value) %>%
  filter(Trait_name == "LMA")

traits <- 
  traits %>% group_by(Species, Time_point) %>% 
  summarise(
    mean_val = mean(Trait_value, na.rm = TRUE),
    sd_val   = sd(Trait_value, na.rm = TRUE),
    cv       = sd_val / mean_val
  )
traits

################################################################################
# Load the demography / phenology data set and tidy a bit
###############################################################################
# I'm going to load this data separately from Lucy's analysis as I'm not sure that she
# has cleaned the LMA data here or not - need to check this with her

data <- read.csv("data/master_data/demography_final_2025_with_phenology_column.csv", stringsAsFactors = FALSE)

# Converting into phenological codes
data <- data %>%
  mutate(`Phenological.stage` = case_when(
    `Phenological.stage` == 1 ~ "Flowering",
    `Phenological.stage` == 0 ~ "Non_Reproductive",
    `Phenological.stage` == 2 ~ "Seeding",
    `Phenological.stage` == 3 ~ "Flowering_Seeding",
    `Phenological.stage` == 4 ~ "Dead",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(`Phenological.stage`))

# Tidy up time names to match those of the trait data
data <- data %>% mutate(Time_point = case_when(Time_point == "May" & Site == "Ainsdale" ~ "T1",
                                         Time_point == "June" & Site == "Ainsdale" ~ "T2",
                                         Time_point == "July" & Site == "Ainsdale" ~ "T3",
                                         Time_point == "May" & Site == "Hazelrigg" ~ "T1",
                                         Time_point == "June" & Site == "Hazelrigg" ~ "T2",
                                         Time_point == "July" & Site == "Hazelrigg" ~ "T3",
                                         Time_point == "April" & Site == "Wytham" ~ "T1",
                                         Time_point == "May" & Site == "Wytham" ~ "T2",
                                         Time_point == "June" & Site == "Wytham" ~ "T3",
                                         Time_point == "July" & Site == "Wytham" ~ "T4"))

# ensuring months are in the correct order 
data$Time_point <- factor(data$Time_point, levels = c("T1", "T2", "T3", "T4"))

# tidy and filter the data set
data <- data %>% 
  dplyr::filter(!Phenological.stage == "Dead") %>%
  dplyr::select(Site, Treatment, Block, Individual_nos, Time_point, Species, Phenological.stage)

# count the number of phenological stages in each species / time category
phen_div <- data %>%
  count(Species, Time_point, Phenological.stage) 

# get a measure of diversity of these stages
phen_div <- phen_div %>% 
  pivot_wider(
    names_from  = Phenological.stage,
    values_from = n,
    values_fill = 0
  )

vars <- phen_div %>% dplyr::select(1:2)
dat <- phen_div %>% dplyr::select(3:6)

################################################################################
# Find the Simpson's diversity index for the phenology stages and join with the LMA variation
###############################################################################

# apply the diversity metrics from vegan
simpson <-  vegan::diversity(as.matrix(dat), index = "simpson",  MARGIN = 1)
diversity <- vars %>% cbind(simpson)

names(traits)
names(diversity)

both <- diversity %>% left_join(traits)
names(both)

################################################################################
# try some simple linear models and plots
###############################################################################

# across all data set
ggplot(both, aes(simpson, cv))+
  geom_point()

# facet this at the species level
ggplot(both, aes(simpson, cv))+
  geom_point()+
  facet_wrap(~ Species)

# very simple linear model but this doesn't control for the wihtin species grouping of variance
mod_1 <- lm(both$simpson ~ both$cv)
summary(mod)

# Now fit a simple LMM
mod_2 <- lmer(cv ~ simpson + (1|Species), data = both, na.action = na.exclude)
summary(mod_2)
model_performance(mod_2)
check_model(mod_2)
r2(mod_2)
parameters(mod_2, df_method = "kenward")
standardize_parameters(mod_2)
report(mod_2)
# the explained variation is at the species level
# no explained variation by the fixed effects 
# we can reject our H3

################################################################################
library(ggeffects)   # for predictions

# Get predicted values from the model
pred <- ggpredict(mod_2, terms = "simpson")

# Plot
mod_plot <- ggplot() +
  geom_point(data = both, aes(simpson, cv), alpha = 0.5) +
  geom_line(data = pred, aes(x, predicted), colour = "#A7CCB8", size = 1.2) +
  geom_ribbon(data = pred, aes(x, ymin = conf.low, ymax = conf.high),
              fill = "#A7CCB8", alpha = 0.5) +
  labs(x = "Phenology diversity (Simpson index)",
       y = "LMA variation (CV)",
       title = "variation in LMA ~ variation in phenological stages + \n(random intercept of species)")+
  theme_classic()
ggsave("figures/mixed_mod_LMA_phen_result.pdf", mod_plot, height = 5, width = 5)


################################################################################


