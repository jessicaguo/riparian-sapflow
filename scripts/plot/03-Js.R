# Plot Js time series
# Fig 3: 9 panels of Js summarized by species

library(ggplot2)
library(ggthemes)

# Read in Js data
load("clean-data/sapflow/Js_daily_sum.Rdata") # Jm_sum

fig_Js <- ggplot(Js_sum, aes(x = as.Date(date), col = site)) + 
  geom_errorbar(aes(ymin = Js_mean - Js_sd, 
                    ymax = Js_mean + Js_sd),
                alpha = 0.3,
                width = 0) +
  geom_point(aes(y = Js_mean)) +
  facet_wrap(~species, scales = "free_y") +
  scale_y_continuous(expression(paste(J[s]," (", g, m^2, s^-1, ")"))) +
  scale_x_date(limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf") +
  facet_wrap(~species, ncol = 3, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = 0)))
fig_Js

jpeg(filename = "plots/Fig3_Js.jpg", width = 7, height = 4, 
     units = "in", res = 600)
print(fig_Js)
dev.off()