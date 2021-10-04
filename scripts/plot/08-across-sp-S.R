# Plot mu.sens and mu.alpha across species/sites
# Also individuals within species/sites?

library(coda)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)

# Load dataset
load("clean-data/sapflow/Gc_daily.Rdata")

# Load CDE
load("clean-data/env/CDE.Rdata")
cde_long <- CDE %>%
  tidyr::pivot_longer(cols = 3:6, names_to = "Site", values_to = "cde")
str(cde_long)

# Find unique sites by species
site_sp <- d %>%
  group_by(species) %>%
  summarize(Site = unique(site))

# Load posterior calculation for muBeta
load(file = "model/coda/jm_coda.Rdata")

# Summarizing chains
sum_tab <- broom.mixed::tidyMCMC(jm_coda, conf.int = TRUE, 
                                 conf.level = 0.95) %>% 
  rename(param = term, mean = estimate, sd = std.error, 
         pc2.5 = conf.low, pc97.5 = conf.high) %>% 
  mutate(sig = ifelse(pc2.5 * pc97.5 > 0, TRUE, FALSE))


# Figure of mu alpha vs. mu m
# mu alpha equivalent to Gref at before VPD exceeds 1 kPa
alpha_int <- sum_tab[grep("mu.alpha\\[\\d{1,2},1\\]", sum_tab$param, perl = TRUE),] %>%
  mutate(Site = site_sp$Site,
         species = site_sp$species,
         alpha_int = mean,
         alpha_low = pc2.5,
         alpha_up = pc97.5) %>%
  select(-1:-6)

mu_m <- sum_tab[grep("mu.sens", sum_tab$param, perl = TRUE),] %>%
  mutate(Site = site_sp$Site,
         species = site_sp$species, by = "species",
         m = abs(mean),
         m_low = abs(pc2.5),
         m_up = abs(pc97.5)) %>%
  select(-1:-6)

alpha_m <- alpha_int %>%
  left_join(mu_m) %>%
  select(-by)

m1 <- lm(m ~ alpha_int, data = alpha_m[-3,])
summary(m1)

fig_m_Gref_1 <- ggplot(data = alpha_m[-3,], aes(x = alpha_int, 
                                          y = m,
                                          color = Site)) +
  geom_errorbar(aes(ymin = m_low, ymax = m_up)) +
  geom_errorbarh(aes(xmin = alpha_low, xmax = alpha_up)) +
  geom_point() +
  scale_y_continuous(expression(paste(mu[m], " (m ", s^-1, log(kPa)^-1, ")"))) +
  scale_x_continuous(expression(paste(G[ref], " (m ", s^-1, ")")),
                     limits = c(2, 10)) +
  scale_color_canva(palette = "Surf and turf") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85, 0.3),
        legend.title = element_blank(),
        legend.background = element_blank())


# Figure of Gref vs. mu m
# Calculate Gref at max CDE equivalent to Gref at before VPD exceeds 1 kPa
cde_max <- cde_long %>%
  group_by(Site) %>%
  summarize(CDEmax = max(cde),
            doymax = which.max(cde))
# Upper had max cde on doy 291, equivalent to October 17, 2004 (leap year)
# Others not until 313 (Nov 8)

# Find cde of each site on doy 291
cde_291 <- cde_long %>%
  filter(doy == 291)

alpha_slope <- sum_tab[grep("mu.alpha\\[\\d{1,2},2\\]", sum_tab$param, perl = TRUE),] %>%
  mutate(Site = site_sp$Site,
         species = site_sp$species) %>%
  left_join(cde_291, by = "Site") %>%
  left_join(alpha_int) %>%
  mutate(Gref = cde*mean + alpha_int,
         Gref_min = cde*pc2.5 + alpha_low,
         Gref_max = cde*pc97.5 + alpha_up) %>%
  left_join(mu_m)

m2 <- lm(m ~ Gref, data = alpha_slope[-3,])
summary(m2)

fig_m_Gref_2 <- ggplot(data = alpha_slope[-3,], aes(x = Gref, 
                                           y = m,
                                           color = Site)) +
  geom_errorbar(aes(ymin = m_low, ymax = m_up)) +
  geom_errorbarh(aes(xmin = Gref_min, xmax = Gref_max)) +
  geom_point() +
  scale_y_continuous(expression(paste(mu[m], " (m ", s^-1, log(kPa)^-1, ")"))) +
  scale_x_continuous(expression(paste(G[ref], " (m ", s^-1, ")")),
                     limits = c(2, 10)) +
  scale_color_canva(palette = "Surf and turf") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85, 0.3),
        legend.title = element_blank(),
        legend.background = element_blank())
