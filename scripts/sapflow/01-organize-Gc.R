# Cleaning and preparation of daily Gc data for Oren-type Bayesian model
# Join with daily and daytime D
# Add CDE

library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)

# Load in daily and daytime Dmean and daily Dmax
Dmean_daily <- read.csv("raw-data/env_site/Dmean_daily.csv")
Dmean_day <- read.csv("raw-data/env_site/Dmean_daytime.csv")
Dmax <- read.csv("raw-data/env_site/Dmax_daily.csv")

# Load in CDE
load("clean-data/env/CDE.Rdata")

# Load site by site

# Jordan
jor <- read.csv("raw-data/Gc_daily/daytime_Gc_jordan.csv", 
              na.strings = "NaN") %>%
  select(-Dmean, -Dmax) %>%
  tidyr::pivot_longer(cols = -1:-2, names_to = "ID", 
                      values_to = "Gc") %>%
  tidyr::separate(ID, sep = 1, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "p" ~ "P. fremontii",
                             species == "t" ~ "T. ramosissima",
                             species == "e" ~ "E. angustifolia"),
         site = "Jordan",
         species = factor(species, levels = c("P. fremontii",
                                              "T. ramosissima",
                                              "E. angustifolia"))) %>%
  left_join(y = Dmean_daily[, c("doy", "Jordan")], by = c("day" = "doy")) %>%
  rename(Dmean_daily = Jordan) %>%
  left_join(y = Dmean_day[, c("doy", "Jordan")], by = c("day" = "doy")) %>%
  rename(Dmean_day = Jordan) %>%
  left_join(y = Dmax[, c("doy", "Jordan")], by = c("day" = "doy")) %>%
  rename(Dmax = Jordan) %>%
  left_join(y = CDE[, c("doy", "Jordan")], by = c("day" = "doy")) %>%
  rename(CDE = Jordan) %>%
filter(complete.cases(.))
str(jor)


ggplot(jor, aes(x = day))+
  geom_point(aes(y = Gc/1000, col = species), alpha = 0.15)+
  geom_line(aes(y=Dmean_daily, col = "Dmean_daily"))+
  geom_line(aes(y=Dmean_day, col = "Dmean_day"))+
  geom_line(aes(y=Dmax, col = "Dmax"))


# Reservoir
res <- read.csv("raw-data/Gc_daily/daytime_Gc_reservoir.csv", 
                na.strings = "NaN") %>%
  select(-Dmean, -Dmax) %>%
  tidyr::pivot_longer(cols = -1:-2, names_to = "ID", 
                      values_to = "Gc") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "ph" ~ "P. hybrid",
                             species == "se" ~ "S. hybrid"),
         site = "Reservoir",
         species = factor(species, levels = c("P. hybrid",
                                              "S. hybrid"))) %>%
  left_join(y = Dmean_daily[, c("doy", "Reservoir")], by = c("day" = "doy")) %>%
  rename(Dmean_daily = Reservoir) %>%
  left_join(y = Dmean_day[, c("doy", "Reservoir")], by = c("day" = "doy")) %>%
  rename(Dmean_day = Reservoir) %>%
  left_join(y = Dmax[, c("doy", "Reservoir")], by = c("day" = "doy")) %>%
  rename(Dmax = Reservoir) %>%
  left_join(y = CDE[, c("doy", "Reservoir")], by = c("day" = "doy")) %>%
  rename(CDE = Reservoir) %>%
  filter(complete.cases(.))
str(res)

ggplot(res, aes(x = day))+
  geom_point(aes(y = Gc/1000, col = species), alpha = 0.15)+
  geom_line(aes(y=Dmean_daily, col = "Dmean_daily"))+
  geom_line(aes(y=Dmean_day, col = "Dmean_day"))+
  geom_line(aes(y=Dmax, col = "Dmax"))


# Todds
tod <- read.csv("raw-data/Gc_daily/daytime_Gc_todds.csv", 
                na.strings = "NaN") %>%
  select(-Dmean, -Dmax) %>%
  tidyr::pivot_longer(cols = -1:-2, names_to = "ID", 
                      values_to = "Gc") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  filter(!ID %in% c(28:29)) %>% # Remove 2 individuals with almost no flow
  mutate(species = case_when(species == "an" ~ "A. negundo"),
         site = "Todds") %>%
  left_join(y = Dmean_daily[, c("doy", "Todds")], by = c("day" = "doy")) %>%
  rename(Dmean_daily = Todds) %>%
  left_join(y = Dmean_day[, c("doy", "Todds")], by = c("day" = "doy")) %>%
  rename(Dmean_day = Todds) %>%
  left_join(y = Dmax[, c("doy", "Todds")], by = c("day" = "doy")) %>%
  rename(Dmax = Todds) %>%
  left_join(y = CDE[, c("doy", "Todds")], by = c("day" = "doy")) %>%
  rename(CDE = Todds) %>%
  filter(complete.cases(.))
str(tod)

ggplot(tod, aes(x = day))+
  geom_point(aes(y = Gc/1000, col = species), alpha = 0.15)+
  geom_line(aes(y=Dmean_daily, col = "Dmean_daily"))+
  geom_line(aes(y=Dmean_day, col = "Dmean_day"))+
  geom_line(aes(y=Dmax, col = "Dmax"))

# Upper
up <- read.csv("raw-data/Gc_daily/daytime_Gc_upper_updated.csv", 
                na.strings = "NaN") %>%
  select(-Dmean, -Dmax) %>%
  tidyr::pivot_longer(cols = -1:-2, names_to = "ID", 
                      values_to = "Gc") %>%
  tidyr::separate(ID, sep = 2, into = c("species", "ID"), 
                  convert = TRUE) %>%
  mutate(species = case_when(species == "ag" ~ "A. grandidentatum",
                             species == "bo" ~ "B. occidentalis",
                             species == "pa" ~ "P. angustifolia"),
         site = "Upper",
         species = factor(species, levels = c("A. grandidentatum",
                                              "B. occidentalis",
                                              "P. angustifolia"))) %>%
  left_join(y = Dmean_daily[, c("doy", "Upper")], by = c("day" = "doy")) %>%
  rename(Dmean_daily = Upper) %>%
  left_join(y = Dmean_day[, c("doy", "Upper")], by = c("day" = "doy")) %>%
  rename(Dmean_day = Upper) %>%
  left_join(y = Dmax[, c("doy", "Upper")], by = c("day" = "doy")) %>%
  rename(Dmax = Upper) %>%
  left_join(y = CDE[, c("doy", "Upper")], by = c("day" = "doy")) %>%
  rename(CDE = Upper) %>%
  filter(complete.cases(.))
str(up)


ggplot(up, aes(x = day))+
  geom_point(aes(y = Gc/1000, col = species), alpha = 0.15)+
  geom_line(aes(y=Dmean_daily, col = "Dmean_daily"))+
  geom_line(aes(y=Dmean_day, col = "Dmean_day"))+
  geom_line(aes(y=Dmax, col = "Dmax"))


# Merge and add date and dt
d <- rbind(jor, res, tod, up)
d$date <- as.POSIXct(paste0(d$year, "-01-01")) + (d$day-1)*24*60*60

# Check assignment of species to sites
table(list(d$species, d$site))

# Provide overall order of species
d$species <- factor(d$species, level = c("P. fremontii", "T. ramosissima", "E. angustifolia",
                                         "P. hybrid", "S. hybrid",
                                         "A. negundo",
                                         "A. grandidentatum", "B. occidentalis", "P. angustifolia"))
str(d)


#make overall figure of Gc by date
fig_Gc_diurnal <- ggplot()+
  geom_point(data = d, aes(x = date, y = Gc, col = as.factor(ID)), alpha = 0.1)+
  scale_x_datetime("Date", date_labels = "%m-%Y")+
  scale_y_continuous(expression(paste(G[c], " (m ", s^-1, ")")))+
  facet_wrap(~species, scales="free")+
  theme_bw(base_size = 12)+
  theme(strip.text = element_text(face = "italic"))+
  scale_color_manual(values = rev(rainbow(30)))+
  guides(col = "none")
fig_Gc_diurnal

#make overall figure of Gc vs Dmax
fig_Gc_D <- ggplot()+
  geom_point(data = d, aes(x = Dmean_day, y = Gc, col = as.factor(ID)), alpha = 0.1)+
  scale_x_continuous(expression(paste(D[max], " (kPa)")))+
  scale_y_continuous(expression(paste(G[c], " (m", s^-1, ")")))+
  facet_wrap(~species, scales = "free_y")+
  theme_bw(base_size = 12)+
  theme(strip.text = element_text(face = "italic"))+
  scale_color_manual(values = rev(rainbow(30)))+
  guides(col= "none")
fig_Gc_D

#test lm with Dmean and Dmax
#no scaling - will be logged in model
m1 <- lm(Gc~log(Dmean_day)*species, data = d)
summary(m1)

m2 <- lm(Gc~log(Dmean_daily)*species, data = d)
summary(m2)

m3 <- lm(Gc~log(Dmax)*species, data = d)
summary(m3)

#perhaps Dmax has a slight edge

#output the collated dataset
save(d, file = "clean-data/sapflow/Gc_daily.Rdata")
