# Plot Js time series
# Fig 3: 9 panels of Js summarized by species

library(ggplot2)
library(ggthemes)

# Read in data
load("clean-data/sapflow/Gc_daily.Rdata") # d
load("clean-data/sapflow/Js_daily_sum.Rdata") # Js_sum
load("clean-data/waterpotential/wp_sp_date.Rdata") # wp_sp_date

# Summarize
Js_sum %>%
  group_by(species) %>%
  summarize(min_Js = min(Js_mean),
            max_Js = max(Js_mean),
            mean_Js = mean(Js_mean)) 

# Add wood anatomy
Js_sum <- Js_sum %>%
  mutate(anatomy = ifelse(species %in% c("T. ramosissima", "E. angustifolia"),
                          "ring", "diffuse"))

# Dataset of water potential measurement dates
js_max <- Js_sum %>%
  group_by(species) %>%
  summarize(y_max = max(Js_mean + Js_sd))

wp_times <- wp_sp_date %>%
  select(species, Date) %>%
  left_join(js_max)

# Remove 3rd set of WP measurements for Jordan
to_remove <- wp_times %>%
  filter(species %in% c("P. fremontii",
                        "T. ramosissima",
                        "E. angustifolia"),
         Date > as.POSIXct("2004-09-01"))

wp_times_removed <- wp_times %>%
  anti_join(to_remove)

fig_Js <- ggplot() + 
  geom_errorbar(data = Js_sum,
                aes(x = as.Date(date),
                    ymin = Js_mean - Js_sd, 
                    ymax = Js_mean + Js_sd,
                    col = site),
                alpha = 0.3,
                width = 0) +
  geom_point(data = Js_sum,
             aes(x = as.Date(date), 
                 y = Js_mean, 
                 col = site,
                 shape = anatomy)) +
  geom_point(data = wp_times_removed,
             aes(x = as.Date(Date),
                 y = y_max),
             pch = 8,
             size = 0.75) +
  facet_wrap(~species, scales = "free_y") +
  scale_y_continuous(expression(paste(J[s]," (g ", m^-2, " ", s^-1, ")"))) +
  scale_x_date(limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf",
                    labels = c("Jordan", 
                               "Reservoir", 
                               "Parley's",
                               "Upper")) +
  scale_shape_manual(values = c(17, 19)) +
  facet_wrap(~species, ncol = 3, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = 0,
                                                  shape = 17)),
         shape = "none")

jpeg(filename = "plots/Fig3_Js.jpg", width = 7, height = 4, 
     units = "in", res = 600)
print(fig_Js)
dev.off()
