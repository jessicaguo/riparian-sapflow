# Original Excel spreadsheets were made machine readable by hand
# Saved as wp_all.csv
# Match to other datasets and summarize in two ways
# 1) by date for each species
# 2) by species only, minimum of each individual, mean minimum of the population

library(tidyr)
library(dplyr)


# read in data
wp <- read.csv("raw-data/waterpotential/wp_all.csv") %>%
  separate(TreeID,
           into = c("tree", "id"),
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(species = case_when(tree == "E" ~ "E. angustifolia",
                             tree == "T" ~ "T. ramosissima",
                             tree == "P" ~ "P. fremontii",
                             tree == "PH" ~ "P. hybrid",
                             tree == "SE" ~ "S. hybrid",
                             tree == "AN" ~ "A. negundo",
                             tree == "AG" ~ "A. grandidentatum",
                             tree == "BO" ~ "B. occidentalis",
                             tree == "PA" ~ "P. angustifolia"),
         species = factor(species, level=c("P. fremontii", "T. ramosissima", "E. angustifolia",
                                           "P. hybrid", "S. hybrid", 
                                           "A. negundo",
                                           "A. grandidentatum", "B. occidentalis", "P. angustifolia"))) %>%
  rename(date = 1) %>%
  mutate(Date = as.POSIXct(date, format = "%m/%d/%Y"))


save(wp, file = "clean-data/waterpotential/wp.Rdata")


# Summarize by species and date
se <- function(x) {sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))}

wp_sp_date <- wp %>%
  group_by(species, Date) %>%
  summarize(n = n(),
            PD_mean = mean(PD),
            MD_mean = mean(MD),
            PD_sd = sd(PD),
            MD_sd = sd(MD),
            PD_se = se(PD),
            MD_se = se(MD),
            delta_mean = mean(MD - PD),
            delta_sd = sd(MD - PD),
            delta_se = se(MD - PD))

save(wp_sp_date, file = "clean-data/waterpotential/wp_sp_date.Rdata")

# Summarize by species only
# Mean minimum WP of each species
# First minimum MD and PD of each individual
# Then population mean, sd, and se
wp_sp <- wp %>%
  mutate(ind = paste0(tree, "_", id)) %>%
  group_by(ind) %>%
  summarize(MD_min = min(MD),
            PD_min = min(PD),
            Site = Site, 
            species= species,
            Date = Date) %>%
  distinct(ind, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(species) %>%
  summarize(n = n(), # number of individuals used to produce the mean minimum
            MD_Mean = mean(MD_min),
            PD_Mean = mean(PD_min),
            MD_sd = sd(MD_min),
            PD_sd = sd(PD_min),
            MD_se = se(MD_min),
            PD_se = se(PD_min))

save(wp_sp, file = "clean-data/waterpotential/wp_sp.Rdata")
