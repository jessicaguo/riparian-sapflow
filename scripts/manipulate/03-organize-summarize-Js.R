# Combine 4 separate Js daily files
# Summarize by species

library(readxl)
library(dplyr)
library(ggplot2)

### Load site by site
# Jordan
list.files("raw-data/Js_daily")
jor <- read_excel("raw-data/Js_daily/daytime_avg_Js_Jordan.xlsx",
                  na = "NaN") %>%
  tidyr::pivot_longer(cols = -1, names_to = "ID", 
                      values_to = "Js") %>%
  tidyr::separate(ID, sep = 1, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "p" ~ "P. fremontii",
                             species == "t" ~ "T. ramosissima",
                             species == "e" ~ "E. angustifolia"),
         site = "Jordan",
         species = factor(species, levels = c("P. fremontii",
                                              "T. ramosissima",
                                              "E. angustifolia"))) %>%
  relocate(site) %>%
    filter(complete.cases(.))
str(jor)

ggplot(jor, aes(x = day, y = Js, col = factor(ID))) + 
  geom_point() +
  facet_wrap(~species)

# Reservoir
res <- read_excel("raw-data/Js_daily/daytime_avg_Js_Reservoir.xlsx", 
                na = "NaN") %>%
  tidyr::pivot_longer(cols = -1, names_to = "ID", 
                      values_to = "Js") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "ph" ~ "P. hybrid",
                             species == "se" ~ "S. hybrid"),
         site = "Reservoir",
         species = factor(species, levels = c("P. hybrid",
                                              "S. hybrid"))) %>%
  relocate(site) %>%
  filter(complete.cases(.))
str(res)

ggplot(res, aes(x = day, y = Js, col = factor(ID))) + 
  geom_point() +
  facet_wrap(~species)

# Todds
tod <- read_excel("raw-data/Js_daily/daytime_avg_Js_Todds.xlsx", 
                na = "NaN") %>%
  tidyr::pivot_longer(cols = -1, names_to = "ID", 
                      values_to = "Js") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  filter(!ID %in% c(28:29)) %>% # Remove 2 individuals with no Gc
  mutate(species = case_when(species == "an" ~ "A. negundo"),
         site = "Todds") %>%
  relocate(site) %>%
  filter(complete.cases(.))
str(tod)

ggplot(tod, aes(x = day, y = Js, col = factor(ID))) + 
  geom_point()+
  facet_wrap(~species)

# Upper
up <- read_excel("raw-data/Js_daily/daytime_avg_Js_Upper_updated.xlsx", 
               na = "NaN") %>%
  tidyr::pivot_longer(cols = -1, names_to = "ID", 
                      values_to = "Js") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "ag" ~ "A. grandidentatum",
                             species == "bo" ~ "B. occidentalis",
                             species == "pa" ~ "P. angustifolia"),
         site = "Upper",
         species = factor(species, levels = c("A. grandidentatum",
                                              "B. occidentalis",
                                              "P. angustifolia"))) %>%
  relocate(site) %>%
  filter(complete.cases(.))
str(up)


ggplot(up, aes(x = day, y = Js, col = factor(ID))) + 
  geom_point()+
  facet_wrap(~species)


### Combine and organize daily Js by individual and date
d <- rbind(jor, res, tod, up) %>%
  mutate(date = as.POSIXct("2004-01-01") + (day-1)*24*60*60,
         species = factor(species, level = c("P. fremontii", 
                                             "T. ramosissima", 
                                             "E. angustifolia",
                                             "P. hybrid", 
                                             "S. hybrid",
                                             "A. negundo",
                                             "A. grandidentatum", 
                                             "B. occidentalis", 
                                             "P. angustifolia"))) %>%
  relocate(site, date)

# Summarize number of daily observations
d %>%
  group_by(species) %>%
  summarize(n = n())

# Summarize to daily Js by species and date
Js_sum <- d %>%
  group_by(species, date) %>%
  summarize(site = unique(site),
            Js_mean = mean(Js),
            Js_sd = sd(Js),
            n = n()) %>%
  relocate(site) %>%
  filter(n >= 3) # Only report dates with at least 3 observations

ggplot(Js_sum, aes(x = date, col = site)) + 
  geom_pointrange(aes(y = Js_mean, 
                      ymin = Js_mean - Js_sd, 
                      ymax = Js_mean + Js_sd, ))+
  facet_wrap(~species, scales = "free_y")

# Save
save(Js_sum, file = "clean-data/sapflow/Js_daily_sum.Rdata")
