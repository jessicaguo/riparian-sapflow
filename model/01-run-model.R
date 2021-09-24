# Model daily Gc as function of Dmax (Oren model)
# allow Gref to vary as a linear combination of CDE
# no centering of logD, so that Gref is equal to conductance at 1 kPa
# hierarchical individual tree within species

library(rjags)
load.module('dic')
library(mcmcplots)
library(data.table)
library(postjags)
library(ggplot2)

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

# list of species to which each individual belongs
sp.ind <- ind %>%
  left_join(sp)


ggplot(d2)+
  geom_histogram(aes(x = log(Dmax)), col = "green", alpha = 0.5, bins = 50)+
  geom_vline(xintercept = 0, col = "red")+
  facet_wrap(~species, nrow = 2)

ggplot(d2, aes(x=log(Dmax), y=Gc/1000, col = as.factor(ID)))+
  geom_vline(xintercept = 0)+
  geom_point()+
  facet_wrap(~species, scales = "free_y", nrow=2)+
  guides(col = "none")


# Create list of input data
datlist <- list(Gc = d2$Gc/1000, # Conductance 
                D =log(d2$Dmax), # Daily maximum VPD
                CDE = d2$CDE, # Cumulative D excess
                ind = d2$ind, # Individual
                sp.ind = sp.ind$sp, # Species identity of each individual
                N = nrow(d2), # N of observations
                Nsp = nrow(sp), # N of species
                Nind = nrow(ind), # N of individuals
                Salpha = matrix(rep(100, 18), ncol = 2), # Sigma for among-species variation in Gref coefficients
                Ssens = rep(100, 9)) # Sigma for among-species variation in sens coefficients ('m')
str(datlist)

# Read initials if available, generate randomly if not

if(file.exists("model/inits/inits.Rdata")){
  # Load prior initials
  load(file="model/inits/inits.Rdata") 
  
  # Specify RNG and seed
  initslist <- list(append(saved.state[[2]][[1]], list(.RNG.name = array("base::Super-Duper"), .RNG.seed = array(23))),
                    append(saved.state[[2]][[2]], list(.RNG.name = array("base::Wichmann-Hill"), .RNG.seed = array(89))),
                    append(saved.state[[2]][[3]], list(.RNG.name = array("base::Mersenne-Twister"), .RNG.seed = array(18))))
  
  
} else {
  # Initials function, use if no prior initials are available
  inits <- function(){
    list(mu.alpha = matrix(rnorm(18, 0, 10), ncol = 2), 
         mu.sens = rnorm(9, 0, 10),
         tau = runif(1, 0, 10),
         tau.eps.alpha = matrix(runif(18, 0, 1), ncol = 2),
         tau.eps.sens = runif(9, 0, 1))
  }
  initslist <- list(inits(), inits(), inits())
}


# Parameters to monitor
params<-c("deviance", "Dsum", # Diagnostics
          "alpha", "sens", # Covariate effects, individual level
          "tau", "sig", # Variance terms, observation level
          "mu.alpha", "mu.sens", # Covariate effects, species level
          "tau.eps.alpha", "tau.eps.sens", # Variance terms, needed for restarting
          "tau.alpha", "sig.alpha", "tau.sens", "sig.sens") # Variance terms, individuals among species

# Initialize and compile model
jm <- jags.model("model/model.jags", data = datlist, inits = initslist, n.chains = 3)

# Updating model (not yet saving results to coda)
# update(jm, n.iter = 10000)

# Run model and monitor output
jm_coda <- coda.samples(jm, variable.names =  params,
                      n.iter = 100000, thin = 100) #results in 1000 its per chain

# Visualize chains for convergence
mcmcplot(jm_coda, parms = c("deviance", "Dsum", 
                            "mu.alpha", "mu.sens", 
                            "sig", "sig.alpha", "sig.sens"))


# Extract final iteration for restarting model
# newinits<-initfind(jm_coda, OpenBUGS = F)
# newinits[[1]]
# saved.state <- removevars(initsin = newinits, variables=c(1:2, 5:8, 10, 13))
# saved.state[[1]]
# save(saved.state, file = "model/inits/inits.Rdata") 

# Save output if visually converged
save(jm_coda, file = "model/coda/jm_coda.Rdata")

# Check convergence with Gelman diagnostic
gel <- gelman.diag(jm_coda, multivariate = F)
save(gel, file = "model/diag/gel.Rdata")

gel$psrf[match("Dsum", row.names(gel$psrf)),]
gel$psrf[match("deviance", row.names(gel$psrf)),]
gel$psrf[match("mu.alpha[1,1]", row.names(gel$psrf)):match("mu.alpha[9,2]", row.names(gel$psrf)),]
which(gel$psrf[match("mu.alpha[1,1]", row.names(gel$psrf)):match("mu.alpha[9,2]", row.names(gel$psrf)),2]>=1.2)

gel$psrf[match("mu.sens[1]", row.names(gel$psrf)):match("mu.sens[9]", row.names(gel$psrf)),]
which(gel$psrf[match("mu.sens[1]", row.names(gel$psrf)):match("mu.sens[9]", row.names(gel$psrf)),2]>=1.2)

gel$psrf[match("sig.alpha[1,1]", row.names(gel$psrf)):match("sig.alpha[9,2]", row.names(gel$psrf)),]
gel$psrf[match("sig.sens[1]", row.names(gel$psrf)):match("sig.sens[9]", row.names(gel$psrf)),]
gel$psrf[match("sig", row.names(gel$psrf)),]


# Run replicated data
jm_rep <- coda.samples(jm, variable.names =  c("Gc.rep"),
                     n.iter = 100000, thin = 100)

save(jm_rep, file = "model/coda/jm_rep.Rdata")
