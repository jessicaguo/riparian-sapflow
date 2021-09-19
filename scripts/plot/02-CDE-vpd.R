# Plot panels of CDE and Dmax by site
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
load("clean-data/Gc_daily.Rdata")
daterange <- data.frame(do.call(rbind, tapply(d$date, d$site, FUN = range))) %>%
  rename(st = X1, en = X2) %>%
  mutate(st = as.Date(as.POSIXct(st, origin = "1970-01-01")),
         en = as.Date(as.POSIXct(en, origin = "1970-01-01"))) %>%
  tibble::rownames_to_column(var = "Site")

fig_env <- ggplot() +
  geom_rect(data = daterange, aes(xmin = st, xmax = en, 
                                  ymin = -Inf, ymax = Inf), alpha = 0.1) +
  geom_point(data = cde_long, aes(x = date, y =  cde, color = Site),
             size = 0.5) +
  geom_point(data = Dmax, aes(x = date, y = Dmax*64), size = 0.5) +
  scale_y_continuous(expression("CDE (kPa" %.% "d)"),
                     sec.axis = sec_axis(~./64, name = "VPD (kPa)")) +
  scale_x_date(limits = c(as.Date("2004-06-01"), as.Date("2004-09-30")),
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_canva(palette = "Surf and turf") +
  facet_wrap(~Site, ncol = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.x=element_blank()) +
  guides(color = "none")

jpeg(filename = "plots/Fig2_CDE_Dmax.jpg", width = 3, height = 6, 
     units = "in", res = 600)
print(fig_env)
dev.off()
