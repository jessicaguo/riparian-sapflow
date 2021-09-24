# Assess model fit
library(coda)
library(dplyr)
library(data.table)
library(ggplot2)

# Load data
# Load dataset
load("clean-data/sapflow/Gc_daily.Rdata")
str(d)

# Make identifying tables
# Numeric species
sp <- d %>%
  group_by(species) %>%
  summarize(species = unique(species)) %>%
  mutate(sp = row_number())

# Consecutive numeric individuals
ind <- d %>%
  mutate(sp_id  = paste0(species, "_", ID)) %>%
  group_by(sp_id) %>%
  summarize(species = unique(species),
            ID = unique(ID),
            sp_id = unique(sp_id)) %>%
  arrange(species, ID) %>%
  mutate(ind = row_number()) %>%
  select(-sp_id)

# Join
d2 <- d %>%
  left_join(sp) %>%
  left_join(ind)

# Load coda for replicated data
load(file="model/coda/jm_rep.Rdata")

# Summarizing chains
sum_rep <- broom.mixed::tidyMCMC(jm_rep, conf.int = TRUE, 
                                 conf.level = 0.95) %>% 
  rename(param = term, mean = estimate, sd = std.error, 
         pc2.5 = conf.low, pc97.5 = conf.high)

#add predicted to d
pred <- data.table(d2, 
                   Gc.pred = sum_rep$mean,
                   Gc.lower = sum_rep$pc2.5,
                   Gc.upper = sum_rep$pc97.5)

#R2
m1 <- lm(Gc.pred~I(Gc/1000), data = pred)
summary(m1)# 0.9547

#coverage
pred$cov <- ifelse(pred$Gc/1000 <= pred$Gc.upper & pred$Gc/1000 >= pred$Gc.lower, 
                   1, 0)
mean(pred$cov) # 0.948

fig_fit <- ggplot(pred) +
  geom_errorbar(aes(x = Gc/1000, ymin = Gc.lower, ymax = Gc.upper), col = "gray") +
  geom_point(aes(x = Gc/1000, y = Gc.pred)) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_abline(slope = coef(summary(m1))[2,1], intercept = coef(summary(m1))[1,1], 
              col = "black", lty = 2) +
  scale_x_continuous(expression(paste("Observed ", G[c]))) +
  scale_y_continuous(expression(paste("Predicted ", G[c]))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  coord_equal()

jpeg(filename = "model/output/fit_all.jpg", width = 4, height = 4, 
     units = "in", res = 600)
print(fig_fit)
dev.off()

# Investigate fit by species
call = list(Gc.pred ~ I(Gc/1000))
fit_byspecies <- pred[, .(m = list(summary(lm(Gc.pred ~ I(Gc/1000))))), 
                      by = species]
getr <- function(x) {x$adj.r.squared}
getslope <- function(x) {x$coef[2,1]}
getint <- function(x) {x$coef[1,1]}
fitsp <- data.frame(species = unique(pred$species),
                    r2 = round(unlist(lapply(fit_byspecies$m, FUN = getr)), 3),
                    slope = round(unlist(lapply(fit_byspecies$m, FUN = getslope)), 3),
                    int = round(unlist(lapply(fit_byspecies$m, FUN = getint)), 3))
fitsp
range(fitsp$r2)

fig_fit_sp <- ggplot() +
  geom_errorbar(data = pred, 
                aes(x = Gc/1000, ymin = Gc.lower, ymax = Gc.upper), col = "gray") +
  geom_point(data = pred, 
             aes(x = Gc/1000, y = Gc.pred)) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_abline(data = fitsp, 
              aes(slope = slope, intercept = int),
              col="black", lty = 2) +
  geom_text(data = fitsp, aes(label = round(r2, 3)), 
            x = Inf, y = -Inf, hjust = 1.1, vjust = -0.2,
            size = 4) +
  scale_x_continuous(expression(paste("Observed ", G[c]))) +
  scale_y_continuous(expression(paste("Predicted ", G[c]))) +
  facet_wrap(~species, scales = "free") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        aspect.ratio = 1)

jpeg(filename = "model/output/fit_byspecies.jpg", width = 6, height = 6, 
     units = "in", res = 600)
print(fig_fit_sp)
dev.off()

# Investigate fit by individual
fit_byind <- pred[, .(m = list(summary(lm(Gc.pred ~ I(Gc/1000))))), 
                  by = .(species, ID)]
fitind <- data.frame(unique(pred[,c("species", "ID")]),
                     r2 = round(unlist(lapply(fit_byind$m, FUN = getr)), 3),
                     slope = round(unlist(lapply(fit_byind$m, FUN = getslope)), 3),
                     int = round(unlist(lapply(fit_byind$m, FUN = getint)), 3)) %>%
  mutate(species = factor(species, levels = levels(d$species))) %>%
  arrange(species, ID)


# Many of P. fremontii have very low (even negative) r2
pred %>%
  filter(species == "P. fremontii") %>%
  ggplot(aes(x = log(Dmax), y = Gc/1000, col = CDE)) +
  geom_point() +
  facet_wrap(~ID)

pred %>%
  filter(species == "P. fremontii") %>%
  ggplot(aes(x = Gc/1000, y = Gc.pred, col = CDE)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point() +
  facet_wrap(~ID)

# Some T. ramosissima also have very low (even negative) r2
pred %>%
  filter(species == "T. ramosissima") %>%
  ggplot(aes(x = log(Dmax), y = Gc/1000, col = CDE)) +
  geom_point() +
  facet_wrap(~ID)

pred %>%
  filter(species == "T. ramosissima") %>%
  ggplot(aes(x = Gc/1000, y = Gc.pred, col = CDE)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point() +
  facet_wrap(~ID)
