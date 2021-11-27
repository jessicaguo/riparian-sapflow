# Plot environment-related variables
# Fig 2: panels of CDE and Dmax by site (double axis plot)
# Fig S1: time-series of water potential by species
# Select colors for sites

library(dplyr)
library(ggplot2)
library(ggthemes)

# Load CDE
load("clean-data/env/CDE.Rdata")
cde_long <- CDE %>%
  tidyr::pivot_longer(cols = 3:6, names_to = "Site", values_to = "cde")
str(cde_long)

# Load daily vpd mean
Dmax <- read.csv("raw-data/env_site/Dmax_daily.csv") %>%
  tidyr::pivot_longer(-1, names_to = "Site", values_to = "Dmax") %>%
  rename(doy = 1) %>%
  mutate(date = as.Date(as.POSIXct((doy-1)*60*60*24, origin = "2004-01-01"))) %>%
  relocate(date)
str(Dmax)

# Obtain ranges for data
load("clean-data/sapflow/Gc_daily.Rdata")
daterange <- data.frame(do.call(rbind, tapply(d$date, d$site, FUN = range))) %>%
  rename(st = X1, en = X2) %>%
  mutate(st = as.Date(as.POSIXct(st, origin = "1970-01-01")),
         en = as.Date(as.POSIXct(en, origin = "1970-01-01"))) %>%
  tibble::rownames_to_column(var = "Site")

# Overwrite site names
site_names <- c(
  'Jordan'="Jordan",
  'Reservoir'="Reservoir",
  'Todds'="Parley's",
  'Upper'="Upper"
)

# Plot double axis of CDE and Dmax
fig_env <- ggplot() +
  geom_rect(data = daterange, aes(xmin = st, xmax = en, 
                                  ymin = -Inf, ymax = Inf), alpha = 0.1) +
  geom_point(data = cde_long, aes(x = date, y =  cde, color = Site),
             size = 0.5) +
  geom_point(data = Dmax, aes(x = date, y = Dmax*64), size = 0.5) +
  scale_y_continuous(expression("CDE (kPa" %.% "d)"),
                     sec.axis = sec_axis(~./64, 
                                         name = expression(paste(D[max], " (kPa)")))) +
  scale_x_date(limits = c(as.Date("2004-06-01"), as.Date("2004-09-30")),
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf") +
  facet_wrap(~Site, ncol = 2, labeller = as_labeller(site_names)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        axis.title.x=element_blank()) +
  guides(color = "none")

jpeg(filename = "plots/Fig2_CDE_Dmax.jpg", width = 6, height = 4, 
     units = "in", res = 600)
print(fig_env)
dev.off()


# Plot WP by time and species

# Load data
load("clean-data/waterpotential/wp_sp_date.Rdata")

# Find unique sites by species
site_sp <- d %>%
  group_by(species) %>%
  summarize(Site = unique(site))

wp_sp_date <- wp_sp_date %>%
  left_join(site_sp) %>%
  mutate(anatomy = ifelse(species %in% c("T. ramosissima", "E. angustifolia"),
                          "ring", "diffuse"))


# Find unique dates by site
wp_sp_date %>%
  group_by(Site) %>%
  summarize(date = unique(Date))


dodge <- position_dodge(width = 0.75)
fig_wp <- ggplot(wp_sp_date, aes(x = Date)) +
  geom_errorbar(aes(y = PD_mean, 
                    ymin = PD_mean - PD_sd,
                    ymax = PD_mean + PD_sd, color = Site), 
                width = 0, position = dodge, alpha = 0.5) +
  geom_point(aes(y = PD_mean, shape = anatomy, color = Site), 
             position = dodge,
             alpha = 0.4,
             size = 1.25) +
  geom_errorbar(aes(y = MD_mean, 
                    ymin = MD_mean - MD_sd,
                    ymax = MD_mean + MD_sd, color = Site), 
                width = 0, position = dodge, alpha = 0.5) +
  geom_point(aes(y = MD_mean, shape = anatomy, color = Site), 
             position = dodge,
             alpha = 1.5,
             size = 1.25) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf",
                    labels = c("Jordan", 
                               "Reservoir", 
                               "Parley's",
                               "Upper")) +
  scale_shape_manual(values = c(17, 19)) +
  facet_wrap(~species) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  guides(shape = "none",
         color = guide_legend(override.aes = list(linetype = 0,
                                                  shape = 17)))

jpeg(filename = "plots/FigS1_WP_ts.jpg", width = 7, height = 4, 
     units = "in", res = 600)
print(fig_wp)
dev.off()
