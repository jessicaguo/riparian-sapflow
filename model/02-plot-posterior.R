# Summarize and plot posteriors
library(coda)
library(dplyr)
library(ggplot2)

# Load dataset
load("clean-data/sapflow/Gc_daily.Rdata")

# Load posterior
load(file = "model/coda/jm_coda.Rdata")

# Summarizing chains
sum_tab <- broom.mixed::tidyMCMC(jm_coda, conf.int = TRUE, 
                                 conf.level = 0.95) %>% 
  rename(param = term, mean = estimate, sd = std.error, 
         pc2.5 = conf.low, pc97.5 = conf.high) %>% 
  mutate(sig = ifelse(pc2.5 * pc97.5 > 0, TRUE, FALSE))

#make figures
labs <- levels(d$species)
alpha.int <- sum_tab[grep("mu.alpha\\[\\d{1,2},1\\]", sum_tab$param, perl = TRUE),]
alpha.int$param <- factor(alpha.int$param, levels = alpha.int$param)
alpha.int$Site <- c(rep("Jordan", 3), 
                    rep("Reservoir", 2), 
                    "Todds", 
                    rep("Upper", 3))
fig_mua <- ggplot(alpha.int) +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, 
                      ymax = pc97.5, color = Site)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous(expression(paste(mu[alpha]))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(hjust = 1, face = "italic"),
        legend.position = c(0.8, 0.8)) +
  coord_flip()

jpeg(filename = "model/output/mua_bysp.jpg", width = 4, height = 4, 
     units = "in", res = 600)
print(fig_mua)
dev.off()

alpha.slope <- sum_tab[grep("mu.alpha\\[\\d{1,2},2\\]", sum_tab$param, perl = TRUE),]
alpha.slope$param <- factor(alpha.slope$param, levels = alpha.slope$param)
alpha.slope$Site <- c(rep("Jordan", 3), 
                      rep("Reservoir", 2), 
                      "Todds", 
                      rep("Upper", 3))
fig_mub <- ggplot(alpha.slope) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, 
                      ymax = pc97.5, color = Site)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous(expression(paste(mu[beta]))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1, face = "italic"),
        legend.position = c(0.8, 0.8)) +
  coord_flip()

jpeg(filename = "model/output/mub_bysp.jpg", width = 4, height = 4, 
     units = "in", res = 600)
print(fig_mub)
dev.off()


# Species-level sensitivity to log(Dmax) (`m` parameter in Oren model)
sens <- sum_tab[grep("mu.sens", sum_tab$param),]
sens$param <- factor(sens$param, levels = sens$param)
ggplot(sens) +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, ymax = pc97.5)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous("Sens") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"))

# Standard deviation among individuals for each species in parameter `m` 
sig.sens <- sum_tab[grep("sig.sens", sum_tab$param),]
sig.sens$param <- factor(sig.sens$param, levels = sig.sens$param)
ggplot(sig.sens) +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, ymax = pc97.5)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous(expression(paste(sigma[sens]))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"))

# Standard deviation among individuals for each species in parameter `alpha[1]` (intercept)
sig.int <- sum_tab[grep("sig.alpha\\[\\d{1,2},1\\]", sum_tab$param, perl = TRUE),]
sig.int$param <- factor(sig.int$param, levels = sig.int$param)
ggplot(sig.int) +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, ymax = pc97.5)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous(expression(paste(sigma[alpha[1]]))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"))

# Standard deviation among individuals for each species in parameter `alpha[2]` (coef of CDE)
sig.slope <- sum_tab[grep("sig.alpha\\[\\d{1,2},2\\]", sum_tab$param, perl = TRUE),]
sig.slope$param <- factor(sig.slope$param, levels = sig.slope$param)
ggplot(sig.slope) +
  geom_pointrange(aes(x = param, y = mean, ymin = pc2.5, ymax = pc97.5)) +
  scale_x_discrete("", labels = labs) +
  scale_y_continuous(expression(paste(sigma[alpha[2]]))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, face ="italic"))
