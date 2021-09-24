### Calculation of time-varying S and Gref
# From CDE and species-level mu.sens
# Output figures of S, Gref, mu_sens, and CDE

library(dplyr)
library(coda)
library(data.table)
library(ggplot2)
library(cowplot)

# Load dataset
load("clean-data/sapflow/Gc_daily.Rdata")

# Load posterior
load("model/coda/jm_coda.Rdata")


# Load CDE (cumulative max daily VPD above 1 kPa)
load("clean-data/env/CDE.Rdata")

# Summarize posterior and extract mu.sens parameters
sum_df <- broom.mixed::tidyMCMC(jm_coda, conf.int = TRUE,
                                conf.level = 0.95) %>%
  rename(param = term, mean = estimate, sd = std.error, 
         pc2.5 = conf.low, pc97.5 = conf.high)

m_df <- sum_df %>%
  filter(grepl("mu.sens", param))

# Collapse mcmc.list into matrix
mcmc <- rbind.data.frame(data.frame(jm_coda[[1]]),
                         data.frame(jm_coda[[2]]),
                         data.frame(jm_coda[[3]]))

# Define function for Bayesian p-values for alpha.slope
pvalue <- function(vec, tail){
  p.lower <- ifelse(vec < 0, 1, 0)
  p.upper <- ifelse(vec > 0, 1, 0)
  
  if (tail == "lower"){
    return(mean(p.lower))
  } else if (tail == "upper"){
    return(mean(p.upper))
  }
}

# Calculate one-tailed Bayesian p-values
sens.T <- mcmc[,grep("mu.alpha.\\d{1,2}.2", colnames(mcmc), perl = TRUE)]
apply(sens.T, 2, pvalue, tail = "lower") # confirm 2 mu.alpha[,2] are significantly > 0
apply(sens.T, 2, pvalue, tail = "upper") # confirm 5 mu.alpha[,2] are significantly < 0

# 7 out of 9 species have gref that vary significantly with CDE

#separate relevant variable into list of 9 species
mcmc.l <- list()
for(i in 1:9){ # number of species
  nms <- c(paste0("mu.sens.", i, "."), 
           paste0("mu.alpha.", i, ".1."),
           paste0("mu.alpha.", i, ".2."))
  inds <- c(grep(nms[1], colnames(mcmc)),
            grep(nms[2], colnames(mcmc)),
            grep(nms[3], colnames(mcmc)))
  mcmc.l[[i]] <- mcmc[, inds]
}

# List of sites to which each species belongs
site.sp <- d %>%
  group_by(species) %>%
  summarize(species = unique(species),
            site = unique(site)) %>%
  mutate(sp = row_number(),
         site_id = as.numeric(as.factor(site)))

# Add sequence of CDE (long format) for each iteration
for(i in 1:9){
  cde_df <- data.frame(matrix(rep(CDE[, c(paste0(site.sp$site[i]))], each = 3000), nrow = 3000))
  mcmc.l[[i]] <- cbind.data.frame(mcmc.l[[i]],cde_df)
}

# Create function to be used within lapply to calculate Sens and Gref
hyd.df <- function(df){
  
  # Function that calculates Sens for a vector (row of mcmc.l)
  hyd.vec <- function(vec){
    end <- length(vec)
    S <- c(-1*vec[1]) / (c(vec[2]) + c(vec[3])*vec[4:end])
    return(S)
  }
  
  # Applies hyd.vec() to a dataframe, by row
  return(t(apply(df, MARGIN = 1, FUN = hyd.vec)))
  
}

gref.df <- function(df){
  
  # Function that calculates gref for a vector (row of mcmc.l)
  gref.vec <- function(vec) {
    end <- length(vec)
    Gref <- c(vec[2]) + c(vec[3])*vec[4:end]
    return(Gref)
  }
  
  # Applies gref.vec() to a dataframe, by row
  return(t(apply(df, MARGIN = 1, FUN = gref.vec)))
  
}

# Lapply to mcmc.l
hyd.mcmc <- lapply(mcmc.l, FUN = hyd.df)
gref.mcmc <- lapply(mcmc.l, FUN = gref.df)

# Create function to summarize mean, median, and central 95% CI, for use within lapply
sum_CI <- function(df){
  mean = apply(df, 2, FUN = mean, na.rm=T)
  median = apply(df, 2, FUN = quantile, probs = c(0.5))
  pc2.5 = apply(df, 2, FUN = quantile, probs = c(0.025))
  pc97.5 = apply(df, 2, FUN = quantile, probs = c(0.975))
  
  return(data.frame(mean, median, pc2.5, pc97.5))
  
}

#lapply to hyd.mcmc
hyd.sum <- lapply(hyd.mcmc, FUN = sum_CI)
gref.sum <- lapply(gref.mcmc, FUN = sum_CI)

#add species and site, then collapse
species <- levels(d$species)

for(i in 1:9){
  hyd.sum[[i]] <- data.frame(site = rep(site.sp$site[i], nrow(hyd.sum[[i]])),
                             species = rep(species[[i]], nrow(hyd.sum[[i]])),
                             day = as.numeric(sub("X", "", row.names(hyd.sum[[i]]))),
                             date = CDE$date,
                             hyd.sum[[i]])
}

for(i in 1:9){
  gref.sum[[i]] <- data.frame(site = rep(site.sp$site[i], nrow(gref.sum[[i]])),
                              species = rep(species[[i]], nrow(gref.sum[[i]])),
                              day = as.numeric(sub("X", "", row.names(gref.sum[[i]]))),
                              date = CDE$date,
                              gref.sum[[i]])
}

hydry <- do.call(rbind, hyd.sum) %>%
  mutate(species = factor(species, levels = levels(d$species)))
Gref <- do.call(rbind, gref.sum) %>%
  mutate(species = factor(species, levels = levels(d$species)))

# Add site and species to dataframe of mu.sens posterior summaries (`m_df`)
m_df$site <- site.sp$site
m_df$species <- site.sp$species

# Plot Sens by day
ggplot(hydry, aes(x = day, y = median)) +
  geom_hline(yintercept = 0.6,lty = 2, lwd = 0.75) +
  geom_errorbar(aes(ymin = pc2.5, ymax = pc97.5, color = site), alpha = 0.1) +
  geom_point(aes(color = site)) +
  # scale_y_continuous(limits = c(0, 2)) +
  scale_y_continuous("Sens") +
  scale_x_continuous("DOY", limits = c(173, 262)) +
  facet_wrap(~species, scale = "free_y") +
  theme_bw()

# Plot Sens by date
ggplot(hydry, aes(x = date, y = median)) +
  geom_hline(yintercept = 0.6,lty = 2, lwd = 0.75) +
  geom_errorbar(aes(ymin = pc2.5, ymax = pc97.5, color = site), alpha = 0.25) +
  geom_point(aes(color = site)) +
  # scale_y_continuous(limits = c(0, 2)) +
  scale_y_continuous("Sens") +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_labels = "%b %d") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw()

# Plot Gref (by date
ggplot(Gref, aes(x = date, y = median)) +
  geom_errorbar(aes(ymin = pc2.5, ymax = pc97.5, color = site), alpha = 0.25) +
  geom_point(aes(color = site)) +
  # scale_y_continuous(limits = c(0, 2)) +
  scale_y_continuous("Gref") +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_labels = "%b %d") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw()

# Plot m (fixed over time)
ggplot(m_df)+
  geom_pointrange(aes(x=species, y=mean, ymin=pc2.5, ymax=pc97.5, col = site))+
  scale_y_continuous("m")+
  theme_bw()+
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(face="italic")) +
  coord_flip()

#save out
save(hydry, file = "model/output/hydry.Rdata")

save(Gref, file = "model/output/Gref.Rdata")

save(m_df, file = "model/output/mu_sens.Rdata")


# Plot CDE for sites, annotate with data duration from each site
cde_long <- CDE %>%
  tidyr::pivot_longer(cols = 3:6, names_to = "Site", values_to = "CDE")

daterange <- data.frame(do.call(rbind, tapply(d$date, d$site, FUN = range)))
colnames(daterange) <- c("st", "en")
daterange$st <- as.Date(as.POSIXct(daterange$st, origin = "1970-01-01"))
daterange$en <- as.Date(as.POSIXct(daterange$en, origin = "1970-01-01"))
daterange$Site <- rownames(daterange)

fig_cde <- ggplot() +
  geom_rect(data = daterange, aes(xmin = st, xmax = en, 
                                  ymin = -Inf, ymax = Inf), alpha = 0.15) +
  geom_point(data = cde_long, aes(x = date, y =  CDE, color = Site)) +
  scale_y_continuous(expression("gdd " (degree*Cd))) +
  scale_x_date(limits = c(as.Date("2004-04-01"), as.Date("2004-10-31")),
               date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(~Site, ncol = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.x=element_blank()) +
  guides(color = "none")

jpeg(filename = "model/output/CDE_bysite.jpg", width = 3, height = 6, 
     units = "in", res = 600)
print(fig_cde)
dev.off()

### add rectangles of date ranges to figure, by species
dr <- rbind(daterange[1,], daterange[1,], daterange[1,],
            daterange[2,], daterange[2,], 
            daterange[3,],
            daterange[4,], daterange[4,], daterange[4,])
dr$species <- factor(levels(d$species), levels = levels(d$species))
hydry$Site <- hydry$site

fig1 <- ggplot() +
  geom_rect(data = dr, aes(xmin = st, xmax = en, 
                           ymin = -Inf, ymax = Inf), alpha = 0.15) +
  geom_hline(yintercept = 0.6,lty = 2, lwd = 0.75) +
  geom_errorbar(data = hydry, aes(x = date,
                                  ymin = pc2.5, ymax = pc97.5, color = Site), 
                alpha = 0.3) +
  geom_point(data = hydry, aes(x = date, y = median, color = Site)) +
  scale_y_continuous(bquote(italic(S))) +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%m/%d") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"))

jpeg(filename = "model/output/sens_byspecies.jpg", width = 7, height = 4, 
     units = "in", res = 600)
print(fig1)
dev.off()


### second version of above figure only showing range of sapflow measurements
# function to cut on hyd.sum
datecut <- function(x, dr){filter(x, date >= dr$st & date <= dr$en)}
dr.list <- split(dr, seq(nrow(dr)))
foo <- datecut(hyd.sum[[1]], dr.list[[1]])
hydry.cut <- mapply(FUN = datecut, x = hyd.sum,  dr = dr.list, SIMPLIFY = FALSE)
hydrycut.df <- do.call(rbind, hydry.cut)

fig2 <- hydrycut.df %>%
  rename(Site = site) %>%
  ggplot() +
  geom_hline(yintercept = 0.6,lty = 2, lwd = 0.75) +
  geom_errorbar(aes(x = date,
                    ymin = pc2.5, ymax = pc97.5, color = Site), 
                alpha = 0.3) +
  geom_point(aes(x = date, y = median, color = Site)) +
  scale_y_continuous(bquote(italic(S))) +
  scale_x_date("Date", limits = range(as.Date(d$date)), 
               date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(~species, scale = "free_y") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"))

jpeg(filename = "model/output/sens_byspecies_cut.jpg", width = 7, height = 4, 
     units = "in", res = 600)
print(fig2)
dev.off()

# For each species, summarize initial, final, and change in Sens during measurement period
sens <- data.frame()
for(i in 1:nrow(dr)){
  st <- dr$st[i]
  en <- dr$en[i]
  sp <- dr$species[i]
  
  temp <- data.frame(species = sp,
                     Site = dr$Site[i],
                     st = st, 
                     en = en,
                     Sens.st = hydry$median[which(hydry$date == st & hydry$species == sp)],
                     Sens.en = hydry$median[which(hydry$date == en & hydry$species == sp)],
                     dSens = hydry$median[which(hydry$date == en & hydry$species == sp)] - 
                       hydry$median[which(hydry$date == st & hydry$species == sp)])
  sens <- rbind.data.frame(sens, temp)
}

save(sens, file = "model/output/Sens_byspecies.Rdata")
