# Plot the calcuated time-varying parameters
# Gref and hydry

library(dplyr)
library(ggplot2)
library(ggthemes)

# Load dataset
load("clean-data/Gc_daily.Rdata")

# Load calculated time-varying hydry and Gref
load(file = "model/output/hydry.Rdata") 
hydry <- hydry %>%
  rename(Site = site)
load(file = "model/output/Gref.Rdata")
Gref <- Gref %>%
  rename(Site = site)

# Obtain range of dates monitored at each site
daterange <- data.frame(do.call(rbind, tapply(d$date, d$site, FUN = range)))
colnames(daterange) <- c("st", "en")
daterange$st <- as.Date(as.POSIXct(daterange$st, origin = "1970-01-01"))
daterange$en <- as.Date(as.POSIXct(daterange$en, origin = "1970-01-01"))
daterange$Site <- rownames(daterange)

### add rectangles of date ranges to figure, by species
dr <- rbind(daterange[1,], daterange[1,], daterange[1,],
            daterange[2,], daterange[2,], 
            daterange[3,],
            daterange[4,], daterange[4,], daterange[4,])
dr$species <- factor(levels(d$species), levels = levels(d$species))


# Plot of hydry, S, or scaled sensitivity
fig_hydry <- ggplot() +
  geom_rect(data = dr, aes(xmin = st, xmax = en, 
                           ymin = -Inf, ymax = Inf), alpha = 0.1) +
  geom_hline(yintercept = 0.6,lty = 2, lwd = 0.55) +
  geom_errorbar(data = hydry, aes(x = date,
                                  ymin = pc2.5, ymax = pc97.5, color = Site), 
                alpha = 0.3) +
  geom_point(data = hydry, aes(x = date, y = median, color = Site)) +
  scale_y_continuous(bquote(italic(S))) +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = 0)))

jpeg(filename = "plots/Fig7_sens_byspecies.jpg", width = 6, height = 4, 
     units = "in", res = 600)
print(fig_hydry)
dev.off()

# Plot of Gref, or denominator of S
fig_gref <- ggplot() +
  geom_rect(data = dr, aes(xmin = st, xmax = en, 
                           ymin = -Inf, ymax = Inf), alpha = 0.1) +
  geom_errorbar(data = Gref, aes(x = date,
                                  ymin = pc2.5, ymax = pc97.5, color = Site), 
                alpha = 0.3) +
  geom_point(data = Gref, aes(x = date, y = median, color = Site)) +
  scale_y_continuous(expression(paste(G[ref]))) +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = 0)))

jpeg(filename = "plots/FigS5_Gref_byspecies.jpg", width = 6, height = 4, 
     units = "in", res = 600)
print(fig_gref)
dev.off()