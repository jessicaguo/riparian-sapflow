# Plot mu Beta, or species-level CDE effect on Gref
# By species, and in conjunction with mean minimum water potentials
# Add shape to distinguish between wood anatomy types

library(coda)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)

# Load dataset
load("clean-data/sapflow/Gc_daily.Rdata")

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


# Figure of mu Beta only
alpha_slope <- sum_tab[grep("mu.alpha\\[\\d{1,2},2\\]", sum_tab$param, perl = TRUE),] %>%
  mutate(Site = site_sp$Site,
         species = site_sp$species,
         anatomy = ifelse(species %in% c("T. ramosissima", "E. angustifolia"),
                          "ring", "diffuse")) 

fig_mub <- ggplot(alpha_slope) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_pointrange(aes(x = fct_rev(species), y = mean, ymin = pc2.5, 
                      ymax = pc97.5, color = Site, shape = anatomy)) +
  scale_y_continuous(expression(paste(mu[beta]))) +
  scale_color_canva(palette = "Surf and turf",
                    labels = c("Jordan", 
                               "Reservoir", 
                               "Parley's",
                               "Upper")) +
  scale_shape_manual(values = c(17, 19)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1, face = "italic"),
        legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  coord_flip() +
  guides(shape = "none",
         color = guide_legend(override.aes = list(linetype = 0,
                                                  shape = 17)))

jpeg(filename = "plots/Fig5_muB_bysp.jpg", width = 4, height = 4, 
     units = "in", res = 600)
print(fig_mub)
dev.off()


# Figure of mu Beta vs. mean minimum WP

# Load WP data
load("clean-data/waterpotential/wp_sp.Rdata")

# Combine wp_sp with alpha.slope
both <- wp_sp %>%
  left_join(alpha_slope) %>%
  select(-sd) %>% # Prevent a naming duplication issue
  tidyr::pivot_longer(cols = 3:8,
                      names_to = c("Type", "measure"),
                      names_pattern = "(.*)_(.*)",
                      values_to = "value") %>%
  tidyr::pivot_wider(names_from = measure,
                     values_from = value) %>%
  rename(mu_beta = mean,
         WP_mean = Mean,
         WP_sd = sd,
         WP_se = se,
         Species = species) %>%
  mutate(Type = case_when(Type == "PD" ~ "Predawn",
                          Type == "MD" ~ "Midday"),
         Type = factor(Type, levels = c("Predawn", "Midday")),
         anatomy = ifelse(Species %in% c("T. ramosissima", "E. angustifolia"),
                          "ring", "diffuse"))

# Calculate linear regression by PD or MD
type <- c("Predawn", "Midday")
fit <- data.frame(Type = character(2),
                  R2 = numeric(2),
                  p = numeric(2),
                  slope = numeric(2),
                  int = numeric(2),
                  R = numeric(2),
                  P = numeric(2))
for(i in 1:2) {
  sub <- subset(both, Type == type[i])
  m <- summary(lm(mu_beta ~ WP_mean, data = sub))
  fvec <- m$fstatistic
  fit$Type[i] <- type[i]
  fit$R2[i] <- round(m$adj.r.squared, 3)
  fit$p[i] <- round(1 - pf(fvec[1], fvec[2], fvec[3]), 3)
  fit$slope[i]<- m$coefficients[2,1]
  fit$int[i]<- m$coefficients[1,1]
  fit$R[i] <- round(cor(sub$mu_beta, sub$WP_mean), 3)
  fit$P[i] <- round(cor.test(sub$mu_beta, sub$WP_mean)$p.value, 3)
}

# Add characteristics for plotting two kinds of labels
fit$lat <- tapply(both$WP_mean, both$Type, max)
fit$lon <- tapply(both$pc97.5, both$Type, max)
fit$lon2 <- tapply(both$pc97.5, both$Type, max) - 0.005
fit$Type <- factor(fit$Type, levels = c("Predawn", "Midday"))
fit$lab <- paste0("italic(R) == ", fit$R)
fit$lab2 <- paste0("italic(p) == ", fit$P)

fig_wp_muB <- ggplot() +
  geom_errorbar(data = filter(both, Type == 'Midday'), 
                aes(x = WP_mean, ymin = pc2.5, ymax = pc97.5, color = Species), 
                width = 0, alpha = 0.5) +
  geom_errorbarh(data = filter(both, Type == 'Midday'),
                 aes(y = mu_beta, xmin = WP_mean - WP_se, xmax = WP_mean + WP_se, color = Species), 
                 height = 0, alpha = 0.5) +
  geom_point(data = filter(both, Type == 'Midday'), aes(x = WP_mean, y = mu_beta, 
                              color = Species, shape = anatomy), 
             size = 2.5) +
  geom_abline(data = filter(fit, Type == 'Midday'), aes(slope = slope, intercept = int), lty = 2) +
  geom_text(data = filter(fit, Type == 'Midday'), aes(x = lat, y = lon, label = lab), hjust = 1, parse = TRUE) + # previously hjust = 0
  geom_text(data = filter(fit, Type == 'Midday'), aes(x = lat, y = lon2, label = lab2), hjust = 1, parse = TRUE) +
  scale_y_continuous(expression(paste(mu[Beta]))) +
  scale_x_continuous(expression(paste(Psi[paste(MD, ",",  min)], " (MPa)"))) +
  scale_color_manual(values = calc_pal()(12)[c(1:8, 12)]) +
  scale_shape_manual(values = c(17, 19)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = c(0.21, 0.285),
        legend.text = element_text(size = 8, face = "italic"),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.4, "cm")) +
  guides(color = guide_legend(override.aes = list(linetype = 0, 
                                                  size = 2,
                                                  shape = c(17, 19, 19,
                                                            17, 17, 17, 
                                                            17, 17, 17))),
         shape = "none")

jpeg(filename = "plots/Fig6_wp_muBeta.jpg", width = 4, height = 4, 
     units = "in", res = 600)
print(fig_wp_muB)
dev.off()

