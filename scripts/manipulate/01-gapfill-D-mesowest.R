### Gapfilling site daily Dmean and Dmax from Olympus Hills
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(plantecophys)
library(lubridate)
library(cowplot)

# Mesowest station, 15-minutely data, Olympus Hills
fn <- "raw-data/env_mesowest/MSI01_2004.csv"
col_names <- names(read_csv(fn, skip = 6, n_max = 0))
oh <- read_csv(fn, col_names = col_names, skip = 8) %>%
  filter(air_temp_set_1 > -15)

# Add date-time and date
oh$dt <- as.POSIXct(substr(oh$Date_Time, 1, 16), format = "%m/%d/%Y %H:%M",
                    tz = "America/Denver")
oh$date <- as.Date(oh$dt, tz = "America/Denver")

# Summarize to daily Tmin and Tmax
oh_sum <- oh %>%
  mutate(vpd = RHtoVPD(relative_humidity_set_1, air_temp_set_1),
         doy = yday(date)) %>%
  group_by(doy) %>%
  summarize(date = unique(date), 
            meanD = mean(vpd),
            maxD = max(vpd), 
            n = n())
 
# Missing data from 2004-11-13; insert and extrapolate from 11-12 an 11-14
oh_gap <- data.frame(date = seq(as.Date("2004-01-01", tz = "America/Denver"),
                                as.Date("2004-12-31", tz = "America/Denver"),
                                by = "day")) %>%
  mutate(doy = yday(date)) %>%
  left_join(y = oh_sum)
ind <- which(is.na(oh_gap$meanD))

oh_gap$meanD[ind] <- (oh_gap$meanD[ind - 1] + oh_gap$meanD[ind + 1]) / 2
oh_gap$maxD[ind] <- (oh_gap$maxD[ind - 1] + oh_gap$maxD[ind + 1]) / 2

# Load Dmean_daily, Dmax_daily from site
Dmean <- read.csv("raw-data/env_site/Dmean_daily.csv", na.strings = "NaN")
Dmax <- read.csv("raw-data/env_site/Dmax_daily.csv", na.strings = "NaN")

# For both Dmean and Dmax at sapflow sites, use RBWS dmean to predict (no max available)
Dmean <- left_join(oh_gap[, c(1,2,3,5)], Dmean, by = "doy")
Dmax<- left_join(oh_gap[, c(1,2,4,5)], Dmax, by = "doy")


# Dmean plots
ggplot(Dmean, aes(x = date)) +
  geom_point(aes(y = meanD, col = "Olympus Hills")) +
  geom_point(aes(y = Jordan, col = "Jordan")) +
  geom_point(aes(y = Reservoir, col = "Reservoir")) +
  geom_point(aes(y = Todds, col = "Todds")) +
  geom_point(aes(y = Upper, col = "Upper")) +
  theme_bw()


ggplot(Dmean) +
  geom_point(aes(x = meanD, y = Jordan, color = "Jordan")) +
  geom_point(aes(x = meanD, y = Reservoir, color = "Reservoir")) +
  geom_point(aes(x = meanD, y = Todds, color = "Todds")) +
  geom_point(aes(x = meanD, y = Upper, color = "Upper")) +
  geom_abline(slope = 1, intercept = 0) +
  coord_equal() +
  theme_bw()

# Dmean models
m1 <- lm(Jordan ~ meanD, data = Dmean)
summary(m1)

m2 <- lm(Reservoir ~ meanD, data = Dmean)
summary(m2)

m3 <- lm(Todds ~ meanD, data = Dmean)
summary(m3)

m4 <- lm(Upper ~ meanD, data = Dmean)
summary(m4)

ggplot(Dmean) +
  geom_point(aes(x = meanD, y = Upper, color = "Upper")) +
  geom_abline(slope = 0.37564, intercept = -0.11198)


# Dmax plots
ggplot(Dmax, aes(x = date)) +
  geom_point(aes(y = maxD, col = "Olympus Hills")) +
  geom_point(aes(y = Jordan, col = "Jordan")) +
  geom_point(aes(y = Reservoir, col = "Reservoir")) +
  geom_point(aes(y = Todds, col = "Todds")) +
  geom_point(aes(y = Upper, col = "Upper")) +
  theme_bw()

ggplot(Dmax) +
  geom_point(aes(x = maxD, y = Jordan, color = "Jordan")) +
  geom_point(aes(x = maxD, y = Reservoir, color = "Reservoir")) +
  geom_point(aes(x = maxD, y = Todds, color = "Todds")) +
  geom_point(aes(x = maxD, y = Upper, color = "Upper")) +
  geom_abline(slope = 1, intercept = 0) +
  coord_equal() +
  theme_bw()

# Dmax models
m5 <- lm(Jordan ~ maxD, data = Dmax)
summary(m5)

m6 <- lm(Reservoir ~ maxD, data = Dmax)
summary(m6)

m7 <- lm(Todds ~ maxD, data = Dmax)
summary(m7)

m8 <- lm(Upper ~ maxD, data = Dmax)
summary(m8)

# Fill in gaps
Dmean_gap <- Dmean %>%
  mutate(Jordan_fit  = predict.lm(m1, newdata = Dmean),
         Jordan_gap = ifelse(is.na(Jordan), Jordan_fit, Jordan),
         Reservoir_fit  = predict.lm(m2, newdata = Dmean),
         Reservoir_gap = ifelse(is.na(Reservoir), Reservoir_fit, Reservoir),
         Todds_fit  = predict.lm(m3, newdata = Dmean),
         Todds_gap = ifelse(is.na(Todds), Todds_fit, Todds),
         Upper_fit  = predict.lm(m4, newdata = Dmean),
         Upper_gap = ifelse(is.na(Upper), Upper_fit, Upper))

Dmax_gap <- Dmax %>%
  mutate(Jordan_fit  = predict.lm(m5, newdata = Dmax),
         Jordan_gap = ifelse(is.na(Jordan), Jordan_fit, Jordan),
         Reservoir_fit  = predict.lm(m6, newdata = Dmax),
         Reservoir_gap = ifelse(is.na(Reservoir), Reservoir_fit, Reservoir),
         Todds_fit  = predict.lm(m7, newdata = Dmax),
         Todds_gap = ifelse(is.na(Todds), Todds_fit, Todds),
         Upper_fit  = predict.lm(m8, newdata = Dmax),
         Upper_gap = ifelse(is.na(Upper), Upper_fit, Upper))

# Check gapfilling visually
# Dmean
g1 <- ggplot(Dmean_gap, aes(x = date)) +
  geom_line(aes(y = meanD, col = "Olympus Hills")) +
  geom_point(aes(y = Jordan_gap, col = "Jordan_gap")) +
  geom_point(aes(y = Jordan, col = "Jordan")) +
  theme_bw() +
  scale_color_manual(values = c("violet", "purple", "gray")) +
  guides(color = "none")

g2 <- ggplot(Dmean_gap, aes(x = date)) +
  geom_line(aes(y = meanD, col = "Olympus Hills")) +
  geom_point(aes(y = Reservoir_gap, col = "Reservoir_gap")) +
  geom_point(aes(y = Reservoir, col = "Reservoir")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

g3 <- ggplot(Dmean_gap, aes(x = date)) +
  geom_line(aes(y = meanD, col = "Olympus Hills")) +
  geom_point(aes(y = Todds_gap, col = "Todds_gap")) +
  geom_point(aes(y = Todds, col = "Todds")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

g4 <- ggplot(Dmean_gap, aes(x = date)) +
  geom_line(aes(y = meanD, col = "Olympus Hills")) +
  geom_point(aes(y = Upper_gap, col = "Upper_gap")) +
  geom_point(aes(y = Upper, col = "Upper")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

plot_grid(g1, g2, g3, g4, ncol = 1)

# Dmax
g5 <- ggplot(Dmax_gap, aes(x = date)) +
  geom_line(aes(y = maxD, col = "Olympus Hills")) +
  geom_point(aes(y = Jordan_gap, col = "Jordan_gap")) +
  geom_point(aes(y = Jordan, col = "Jordan")) +
  theme_bw() +
  scale_color_manual(values = c("violet", "purple", "gray")) +
  guides(color = "none")

g6 <- ggplot(Dmax_gap, aes(x = date)) +
  geom_line(aes(y = maxD, col = "Olympus Hills")) +
  geom_point(aes(y = Reservoir_gap, col = "Reservoir_gap")) +
  geom_point(aes(y = Reservoir, col = "Reservoir")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

g7 <- ggplot(Dmax_gap, aes(x = date)) +
  geom_line(aes(y = maxD, col = "Olympus Hills")) +
  geom_point(aes(y = Todds_gap, col = "Todds_gap")) +
  geom_point(aes(y = Todds, col = "Todds")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

g8 <- ggplot(Dmax_gap, aes(x = date)) +
  geom_line(aes(y = maxD, col = "Olympus Hills")) +
  geom_point(aes(y = Upper_gap, col = "Upper_gap")) +
  geom_point(aes(y = Upper, col = "Upper")) +
  theme_bw() +
  scale_color_manual(values = c("gray", "violet", "purple")) +
  guides(color = "none")

plot_grid(g5, g6, g7, g8, ncol = 1)

# Save Dmax and Dmean for gapfilled timeseries only
Dmean_out <- Dmean_gap %>%
  select(date, doy, Jordan_gap, Reservoir_gap, Todds_gap, Upper_gap) %>%
  rename(Jordan = Jordan_gap,
         Reservoir = Reservoir_gap,
         Todds = Todds_gap,
         Upper = Upper_gap)

Dmax_out <- Dmax_gap %>%
  select(date, doy, Jordan_gap, Reservoir_gap, Todds_gap, Upper_gap) %>%
  rename(Jordan = Jordan_gap,
         Reservoir = Reservoir_gap,
         Todds = Todds_gap,
         Upper = Upper_gap)

save(Dmean_out, file = "clean-data/env/Dmean.Rdata")
save(Dmax_out, file = "clean-data/env/Dmax.Rdata")

# Calculate daily VPD > 1 kPa, with maxD
cde_daily <- data.frame(date = Dmax_out$date,
                        doy = Dmax_out$doy,
                        Jordan = ifelse(Dmax_out$Jordan > 1, Dmax_out$Jordan - 1, 0),
                        Reservoir = ifelse(Dmax_out$Reservoir > 1, Dmax_out$Reservoir - 1, 0),
                        Todds = ifelse(Dmax_out$Todds > 1, Dmax_out$Todds - 1, 0),
                        Upper = ifelse(Dmax_out$Upper > 1, Dmax_out$Upper - 1, 0))

# Sum to cumulative VPD in excess of 1 kPa
CDE <- data.frame(date = cde_daily$date,
                  doy = cde_daily$doy,
                  Jordan = cumsum(cde_daily$Jordan),
                  Reservoir = cumsum(cde_daily$Reservoir),
                  Todds = cumsum(cde_daily$Todds),
                  Upper = cumsum(cde_daily$Upper))

CDE %>% tidyr::pivot_longer(3:6, names_to = "Site", values_to = "CDE") %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = CDE, color = Site)) + 
  theme_bw(base_size = 12)

save(CDE, file = "clean-data/env/CDE.Rdata")
