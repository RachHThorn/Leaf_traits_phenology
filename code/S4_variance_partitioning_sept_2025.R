# R Thornley
# 18/09/2025
# Variance partitioning methods
# calculate variance in a data set according to different levels of organisation
# using random effects models

library(lme4)
library(tidyverse)
library(purrr)
library(scales)

################################################################################
# INSTRUCTIONS
################################################################################

# 1) Load trait data
# 2) Build a RE model for one trait and species to determine the method
# here we decompose the variance within each trait and species by the following:
# species (inter-specific variation),
# sampling times within species (intra-specific variation across time)
# individual variation within species (intra-specific variation at the individual level)
# treatment variation within species (intra-specific variation across disturbance regimes)

# 3) RUN the RE model for all the species and traits in the data set
# 4) Examine why there are singular models
# 5) For one species re-run code so that singular values are returned as zeros
# 6) Generalise 5) for each species using robust code - to get 
# 7) nice violin plot for the variance decomposition for each species
# 8) Now build the models across all the species and try out different RE structures

################################################################################
# 1) Load the cleaned field trait data
################################################################################

# load the trait dat
traits <- read_csv("results/Field_dry_trait_data_with_flags.csv") 
# filter out the outliers which are flagged from the old script
traits <- traits %>% filter(is_outlier == FALSE)

################################################################################
# 2) PROCESS RE models for the field trait data FOR ONE SPECIES AND TRAIT (TEST)
################################################################################

# look at the species and traits we have 
unique(traits$Species) # CA
unique(traits$Trait_name) # LMA, LDMC, LWC, EWT

traits$Date
traits$Time_point
traits$Individual_nos
# SIDE: if I try to add within individual variance I get a singular model within species

# try for species CA and leaf trait LDMC
LDMC <- traits %>% 
  filter(Species == "CA") %>% 
  filter(Trait_name == "LDMC") %>%
  lmer(Trait_value ~ 1 + 
                   (1|Time_point) +
                   (1|Treatment),
                 data = .)


vc_LDMC <- as.data.frame(VarCorr(LDMC)) |>
  dplyr::select(grp, vcov) |>
  dplyr::mutate(
    prop  = vcov/sum(vcov),
    trait = "LDMC",
    species = "CV"
  )

# plot out as a stacked barchart
ggplot(vc_LDMC, aes(x = prop, y = trait, fill = grp)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_x_continuous()+
  labs(x = NULL, y = "Variance share", fill = "Component",
       title = "Variance partitioning for LDMC") +
  theme_minimal()


################################################################################
# 3) RUN the RE model for all the species and traits in the data set
################################################################################

# data input
traits <- read_csv("results/Field_dry_trait_data_with_flags.csv") 
names(traits)
traits <- traits %>% filter(is_outlier == FALSE)

# inputs
species_list <- unique(traits$Species)
traits_list  <- c("LMA","LDMC","LWC","EWT")

# simple grid of combos
grid <- tidyr::expand_grid(Species = species_list, Trait_name = traits_list)

# one small worker that stays tidy
fit_vc <- function(sp, tr) {
  dat <- traits %>%
    filter(Species == sp, Trait_name == tr) %>%
    mutate(
      Time_point = as.factor(Time_point),
      Treatment = as.factor(Treatment)
    )
  
  if (nrow(dat) < 5) return(tibble(Species = sp, Trait_name = tr, grp = NA, vcov = NA_real_))
  
  mod <- tryCatch(
    lmer(Trait_value ~ 1 + (1|Time_point) + (1|Treatment), data = dat),
    error = function(e) NULL
  )
  
  if (is.null(mod) || isSingular(mod)) {
    return(tibble(Species = sp, Trait_name = tr,
                  grp = NA_character_, vcov = NA_real_, prop = NA_real_))
  }
  
  vc <- as.data.frame(VarCorr(mod)) %>%
    select(grp, vcov)
  
  # lmer already includes Residual; no need to add it again
  vc %>%
    mutate(Species = sp, Trait_name = tr, .before = 1)
}

# run across grid and compute shares
all_vc <- pmap_dfr(grid, \(Species, Trait_name) fit_vc(Species, Trait_name)) %>%
  group_by(Species, Trait_name) %>%
  mutate(prop = vcov / sum(vcov, na.rm = TRUE)) %>%
  ungroup()

ggplot(all_vc, aes(x = Trait_name, y = prop, fill = grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  facet_wrap(~ Species, scales = "free_x") +
  theme_minimal()

# THERE ARE QUITE A FEW SNGULAR MODELS THAT NEED DEALING WITH

###############################################################################
# 4) LOOK AT SINGULAR MODELS
##############################################################################

# There are 14 singular models
# filter the results from stage 3) for the singular models
df_na <- all_vc %>% filter(if_any(everything(), is.na))
df_na

# Look at which models have singular fits
sing_species <- unique(df_na$Species)
sing_species
sing_traits <- unique(df_na$Trait_name)
sing_traits

# Look at one example: looks like this sing. fit is due to the Treatment having no 
# variance associated with it
mod <- traits %>% filter(Species == "MA" & Trait_name == "LMA") %>%
  lmer(Trait_value ~ 1 + (1|Time_point) + (1|Treatment), data = .)
isSingular(mod)
as.data.frame(VarCorr(mod))

# Just check there is some data there
traits %>% filter(Species == "MA" & Trait_name == "LMA") %>% 
  group_by(Treatment, Time_point) %>% tally()

# some other methods to get info on the reasons for singular fits
# replace x with your predictor used in random slopes
traits %>%
  group_by(Time_point) %>%
  summarise(Trait_value = sd(Trait_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(Trait_value)  # many zeros? then (1 + x | Time_point) is unidentified
# no zeros here

# look for where rank deficiency lies
rePCA(mod)

################################################################################
# 5) RUN CODE FOR SING MODELS: RETURN VAR COMPS OF ZERO: ONE SPECIES: ROBUST CODE
################################################################################

# data input
traits <- read_csv("results/Field_dry_trait_data_with_flags.csv") 
names(traits)
traits <- traits %>% filter(is_outlier == FALSE)


# --- helper: floor tiny numbers to 0
zero_small <- function(x, tol = 1e-10) ifelse(x < tol, 0, x)

# --- fit + extract variance components, zeroing singular parts
vc_zeroed <- function(formula, data, tol = 1e-10) {
  mod <- tryCatch(lmer(formula, data = data), error = function(e) NULL)
  
  if (is.null(mod)) {
    return(tibble(grp = NA_character_, vcov = NA_real_, prop = NA_real_, status = "fit-error"))
  }
  
  # components from VarCorr; lmer already includes Residual
  vc <- as.data.frame(VarCorr(mod)) %>%
    select(grp, vcov)
  
  # force exact zeros for tiny variances
  vc <- vc %>% mutate(vcov = zero_small(vcov, tol))
  
  # (optional) if some component is NA (rare), set to 0 as well
  vc$vcov[is.na(vc$vcov)] <- 0
  
  # proportions
  tot <- sum(vc$vcov)
  vc <- vc %>% mutate(prop = if (tot > 0) vcov/tot else NA_real_)
  
  # status tag
  status <- if (isSingular(mod)) "singular->zeroed" else "ok"
  
  vc %>% mutate(status = status)
}

# --- Example: your structure
# Make sure grouping vars are factors
traits <- traits %>%
  mutate(Time_point = as.factor(Time_point),
         Treatment  = as.factor(Treatment))

dat <- traits %>% filter(Species == "MA" & Trait_name == "LMA")

out <- vc_zeroed(
  Trait_value ~ 1 + (1|Time_point) + (1|Treatment),
  data = dat
)

# now we have props for all 
out

####################################################################################
# 6) GENERALISE 5) ACROSS ALL TRAITS WITHIN A SPECIES FOR SING MODELS: ROBUST CODE
####################################################################################

# There are 14 singular models produced from the last lot of code
# filter the data for the singular models
df_na <- all_vc %>% filter(if_any(everything(), is.na))
df_na

# Look at which models have singular fits
species_list <- unique(df_na$Species)
traits_list <- unique(df_na$Trait_name)

# Check if I can run this for all models no matter if they are singular or not
species_list <- unique(traits$Species)
traits_list <- unique(traits$Trait_name)

grid <- tidyr::expand_grid(Species = species_list, Trait_name = traits_list)

fit_one <- function(sp, tr, tol = 1e-10) {
  df <- traits %>%
    filter(Species == sp, Trait_name == tr) %>%
    mutate(Time_point = as.factor(Time_point),
           Treatment  = as.factor(Treatment))
  if (nrow(df) < 5) {
    return(tibble(Species = sp, Trait_name = tr, grp = NA, vcov = NA_real_, prop = NA_real_, status = "too-few"))
  }
  
  vc <- vc_zeroed(Trait_value ~ 1 + (1|Time_point) + (1|Treatment), data = df, tol = tol)
  vc %>% mutate(Species = sp, Trait_name = tr, .before = 1)
}

all_vc <- pmap_dfr(grid, \(Species, Trait_name) fit_one(Species, Trait_name))
all_vc

ggplot(all_vc, aes(x = Trait_name, y = prop, fill = grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  facet_wrap(~ Species, scales = "free_x") +
  theme_minimal()

# save this for later
# write_csv(all_vc, "results/var_com_ind_species.csv")

# we still don't have any results for SR
# have a look why this might be
traits %>% 
  filter(Species == "SR" & Trait_name == "LMA") %>%
  ggplot(aes(Treatment, Trait_value))+
  geom_boxplot()+
  facet_wrap(~Time_point)

# This is because it does not feature in any other plots other than control!!
# we could take this out of the results for now but will need to think of a way 
# to deal with this data at some point

###############################################################################
# 7) SPECIES LEVEL VARIANCE DECOMPOSITION FIGURE 1: VIOLINS WITH SPECIES DOTS
################################################################################

# get rid of SR as we don't have this species in a disturbance plot (it's extinct)
plot_dat <- all_vc %>% filter(!grp == "SR")
plot_dat <- plot_dat %>%
  mutate(grp = recode(grp, "Treatment" = "Disturbance regime",
                      "Time_point" = "Phenology",
                      "Residual"  = "Unexplained intraspecfic"))

# save this plot data to finalise plot in other script
# write_csv(plot_dat, "results/var_comp_plot_dat.csv")
names(plot_dat)
unique(plot_dat$Trait_name)
plot <- plot_dat %>%
  filter(Trait_name %in% c("LMA", "EWT", "LDMC")) %>%
  ggplot(aes(grp, prop)) + theme_bw() + geom_violin() + geom_point(aes(colour = Species), size = 5) +
  facet_wrap(~ Trait_name, ncol = 4)+
  theme(strip.background = element_blank(),   
        strip.text = element_text(face = "bold"))+
  theme(legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3)))+
  geom_text(aes(label = Species, colour = Species), 
            vjust = -1, size = 3, show.legend = FALSE)  # labels above points
plot
ggsave("figures/var_comp_figure_labelled.jpeg", plot, height = 7, width = 7)

# NOTE: needs working on: we could replace dots with species specific symbols for example

################################################################################
# 8) VAR COMP FOR ALL SPECIES ACROSS ALL SITES
################################################################################

# data input
traits <- read_csv("results/Field_dry_trait_data_with_flags.csv") 
names(traits)
traits <- traits %>% filter(is_outlier == FALSE)

# Build the random effects models for each trait 
# across all species

# RE structure 1
# using treatment, site, sampling date, species as the REs
var_str_1 <- traits %>% group_by(Trait_name) %>% nest() %>% 
  mutate(models = map(data, ~ lmer(Trait_value ~ 1 + (1 | Time_point) + (1 | Treatment) + (1 | Site) + ( 1 | Species), 
                                   data = .x)),
         vars = map(models, ~ as.data.frame(VarCorr(.x)))) %>%
  dplyr::select(Trait_name, vars) %>%
  unnest(cols = c(vars)) %>%
  mutate(new_grp = case_when(grp == "Species" ~ "Between species",
                             grp == "Treatment" ~ "Between treatments",
                             grp == "Site" ~ "Between sites",
                             grp == "Time_point" ~ "Between sampling dates",
                             grp == "Residual" ~ "Unexplained variation")) %>%
  group_by(Trait_name) %>%
  mutate(prop = vcov / sum(vcov, na.rm = TRUE)) %>%
  ungroup()

# visualise this as a bar plot
ggplot(var_str_1, aes(x = Trait_name, y = prop, fill = new_grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  theme_minimal()

# RE structure 2
# drop site (as it does nothing) - try testing treatment within species 
# then species and time as the REs
var_str_2 <- traits %>% group_by(Trait_name) %>% nest() %>% 
  mutate(models = map(data, ~ lmer(Trait_value ~ 1 + ( 1 | Species) + (1 | Species:Treatment) + 
                                     (1| Time_point), 
                                   data = .x)),
         vars = map(models, ~ as.data.frame(VarCorr(.x)))) %>%
  dplyr::select(Trait_name, vars) %>%
  unnest(cols = c(vars)) %>%
  mutate(new_grp = case_when(grp == "Species" ~ "Between species",
                             grp == "Species:Treatment" ~ "Treatments within species",
                             grp == "Time_point" ~ "Between sampling dates",
                             grp == "Residual" ~ "Leaf / sample level variation")) %>%
  mutate(prop = vcov/sum(vcov))

# visualise this as a bar plot
ggplot(var_str_2, aes(x = Trait_name, y = prop, fill = new_grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  theme_minimal()


# RE structure 3
# still treat
var_str_2 <- traits %>% group_by(Trait_name) %>% nest() %>% 
  mutate(models = map(data, ~ lmer(Trait_value ~ 1 + ( 1 | Species) + (1 | Species:Treatment) + 
                                     (1| Time_point), 
                                   data = .x)),
         vars = map(models, ~ as.data.frame(VarCorr(.x)))) %>%
  dplyr::select(Trait_name, vars) %>%
  unnest(cols = c(vars)) %>%
  mutate(new_grp = case_when(grp == "Species" ~ "Between species",
                             grp == "Species:Treatment" ~ "Treatments within species",
                             grp == "Time_point" ~ "Between sampling dates",
                             grp == "Residual" ~ "Unexplained variation"))%>%
  mutate(prop = vcov/sum(vcov))

# visualise this as a bar plot
ggplot(var_str_2, aes(x = Trait_name, y = prop, fill = grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  theme_minimal()


################################################################################
# VAR COMP FOR ALL : not sure what this code is doing at the moment
#################################################################################

# plot rough

# You can generalise over lots of traits

# plot out the results
plot <- ggplot(dat, aes(x = trait, y = prop, fill = grp)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Proportion of Variance",
       title = "Variance Partitioning of Plant Traits in Cirsium arvense",
       fill = "Source of Variation") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
plot
ggsave("figures/variance_decomp_CA.jpeg", plot, height = 5, width = 8)

################################################################################
