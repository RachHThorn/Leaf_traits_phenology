# R Thornley
# 02/12/2025
# Variance partitioning methods
# calculate variance in a data set according to different levels of organisation
# using random effects models
# for LMA only for the BES conference (Lucy's poster)

library(lme4)
library(tidyverse)
library(purrr)
library(scales)

################################################################################
# LOAD and TIDY DATA 
################################################################################

# load the trait data
traits <- read_csv("results/Field_dry_trait_data_with_flags.csv") 
# filter out the outliers which are flagged from the old script
traits <- traits %>% filter(is_outlier == FALSE)
# make the data set small with only the variables and traits we need
names(traits)
traits <- traits %>% 
  dplyr::select(Site, Treatment, Date, Time_point, Species, Trait_name, Trait_value) %>%
  filter(Trait_name == "LMA")


################################################################################
# Build RE models version 1
################################################################################

# all species 
mod <- lmer(Trait_value ~ 1 + ( 1 | Species) + (1 | Species:Treatment) + (1| Time_point), data = traits)
vc <- as.data.frame(VarCorr(mod))
vc <- vc %>% 
  mutate(
    new_grp = case_when(
      grp == "Species"           ~ "Between species",
      grp == "Species:Treatment" ~ "Treatments within species",
      grp == "Time_point"        ~ "Between sampling dates",
      grp == "Residual"          ~ "Leaf / sample level variation",
      TRUE                       ~ grp
    ),
    prop = vcov / sum(vcov),
    Trait_name = "LMA"   # or `trait_name` if that’s what your other code uses
  )

# visualise this as a bar plot
ggplot(vc, aes(x = Trait_name, y = prop, fill = new_grp)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Variance share", fill = "Component") +
  theme_minimal()

################################################################################
# Build RE models version 2
################################################################################

# all species 
mod <- lmer(Trait_value ~ 1 + ( 1 | Species) + (1 | Species:Treatment) + 
              (1| Species:Time_point), data = traits)
vc <- as.data.frame(VarCorr(mod))
vc <- vc %>% 
  mutate(
    new_grp = case_when(
      grp == "Species"             ~ "Between species",
      grp == "Species:Treatment"   ~ "Treatments within species",
      grp == "Species:Time_point"  ~ "Phenology within species",
      grp == "Residual"            ~ "Unexplained individual level variation",
      TRUE                         ~ grp
    ),
    prop = vcov / sum(vcov),
    Trait_name = "LMA"   # or `trait_name` if that’s what your other code uses
  )

# with some lighter shaded colours
# visualise this as a bar plot
p1 <- ggplot(vc, aes(x = Trait_name, y = prop, fill = new_grp)) +
  geom_col(width = 1) +
  geom_text(
    aes(label = paste0(new_grp, " (", percent(prop, accuracy = 1), ")")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Percent variance", fill = "Source of variance") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank()
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c(
      "Between species" = "#A7CCB8",
      "Phenology within species" = "#93C9CD",
      "Treatments within species" = "#E7D67F",
      "Unexplained individual level variation" = "#D8889B"))
p1

ggsave("figures/BES_poster_Lucy/Site_level_var_decomp.pdf", p1, height = 12, width = 4)

# also without any text
# visualise this as a bar plot
p2 <- ggplot(vc, aes(x = Trait_name, y = prop, fill = new_grp)) +
  geom_col(width = 1) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Percent variance", fill = "Source of variance") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank()
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c(
      "Between species" = "#A7CCB8",
      "Phenology within species" = "#93C9CD",
      "Treatments within species" = "#E7D67F",
      "Unexplained individual level variation" = "#D8889B"))
p2

ggsave("figures/BES_poster_Lucy/Site_level_var_decomp_no_labels.pdf", p2, height = 12, width = 4)


################################################################################
# now visualise in a similar way for the individual species models
################################################################################


all_sp <- read_csv("results/var_com_ind_species.csv")
names(all_sp)
# get rid of SR for now as it si only foudn in one treatment
all_sp <- all_sp %>% filter(!Species == "SR")

p3 <- ggplot(all_sp, aes(x = Trait_name, y = prop, fill = grp)) +
  geom_col(width = 1) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Trait", y = "Percent variance", fill = "Source of variance") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank()
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c(
      "Between species" = "#A7CCB8",
      "Phenology within species" = "#93C9CD",
      "Treatments within species" = "#E7D67F",
      "Unexplained individual level variation" = "#D8889B"))+
  facet_wrap(~ Species)
p3


################################################################################
# 
################################################################################


values = c(
  "#738FC1",
  "#C1DDEB",
  "#F3EAD3",
  "#E7CBA9",
  "#C79C66",
  "#9D6B54",
  "#6C4E4C"
)

values = c(
  "#002147",  # Oxford Blue
  "#4F8F6E",  # Biology Green
  "#0F7C82",  # Oxford Teal
  "#C4A000", # Oxford Gold
  "#A50034" # Oxford Magenta
)

# some pastel versions of these colours
values_pastel <- c(
  "Between species"             = "#8094AD",
  "Treatments within species"   = "#A7CCB8",
  "Between sampling dates"      = "#93C9CD",
  "Leaf / sample level variation" = "#E7D67F",
  "Residual"                    = "#D8889B"
)

# light background pastel tone
"#E8E3D3"


