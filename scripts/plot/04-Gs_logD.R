# Plot panels of Gs vs log(D) by species
# Select colors for sites
library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)

# Load dataset
load("clean-data/Gc_daily.Rdata")

# First, show Oren relationship with day of year as color axis
fig_Gc_logD <- ggplot() +
  geom_point(data = d, aes(x = log(Dmax), y = Gc/1000, color = day), alpha = 0.2) +
  scale_x_continuous(expression(paste("log(", D[max], ")")),
                     breaks = 0:2) +
  scale_y_continuous(expression(paste(G[c]))) +
  scale_color_continuous_tableau(name = "Day of year", 
                                 palette = "Green-Gold", 
                                 trans = 'reverse') +
  facet_wrap(~species, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.title = element_text(size = 10))

jpeg(filename = "plots/Fig4_Gc_logD.jpg", width = 6, height = 4, 
     units = "in", res = 600)
print(fig_Gc_logD)
dev.off()

# Then, select 2 species, perhaps 4 individuals each, and show CDE on color axis
fig_Gc_logD_inds_A <- d %>%
  filter(species %in% c("E. angustifolia") &
           ID %in% c(1:2, 5:6)) %>%
  ggplot() +
  geom_point(aes(x = log(Dmax), y = Gc/1000, color = CDE)) +
  scale_x_continuous(expression(paste("log(", D[max], ")")),
                     breaks = seq(0.5, 2, by = 0.5)) +
  scale_y_continuous(expression(paste(G[c]))) +
  scale_color_gradientn(name = "CDE", 
                        colors = c("coral", "yellowgreen"), 
                        trans = "reverse") +
  facet_grid(rows = vars(species), 
             cols = vars(ID), 
             scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.title = element_text(size = 10))

fig_Gc_logD_inds_B <- d %>%
  filter(species %in% c("A. negundo") &
           ID %in% c(6:7, 16:17)) %>%
  ggplot() +
  geom_point(aes(x = log(Dmax), y = Gc/1000, color = CDE)) +
  scale_x_continuous(expression(paste("log(", D[max], ")")),
                     breaks = seq(0.5, 2, by = 0.5)) +
  scale_y_continuous(expression(paste(G[c]))) +
  scale_color_gradientn(name = "CDE", 
                        colors = c("coral", "yellowgreen"), 
                        trans = "reverse") +
  facet_grid(rows = vars(species), 
             cols = vars(ID), 
             scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.title = element_text(size = 10))

jpeg(filename = "plots/FigS1_Gc_logD_ind.jpg", width = 8, height = 4, 
     units = "in", res = 600)
plot_grid(fig_Gc_logD_inds_A, fig_Gc_logD_inds_B, labels="auto", ncol = 1)
dev.off()