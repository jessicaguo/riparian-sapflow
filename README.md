# riparian-sapflow
## Temporal variation in stomatal sensitivity to vapor pressure deficit in western riparian forests
Authors: Jessica S. Guo, Susan E. Bush, Kevin R. Hultine

Data and code associated with Guo, Bush, and Hultine (2022) in Functional Ecology

### Abstract
1. Increasing atmospheric vapor pressure deficit (D) can influence plant water and carbon uptake. However, growing season variation in stomatal responses to D among tree taxa has not been thoroughly quantified and therefore has not been well characterized in stomatal regulation models. 
2. Using sap-flux data from nine riparian species spanning a 600 m elevation gradient in semi-arid northern Utah, USA, we fit a time-varying empirical model of stomatal conductance to D in a hierarchical Bayesian framework. The reference conductance (G<sub>ref</sub>, conductance at D = 1 kPa) term was modeled as a function of cumulative growing season D, which varied with site elevation.
3. Seven species exhibited Gref that varied significantly with cumulative growing-season D, but the direction was not consistent among species. Two low-elevation ring-porous species, the invasive <i>Tamarix ramosissima</i> and <i>Elaeagnus angustifolia </i>, exhibited significantly positive correlation between G<sub>ref</sub> and cumulative D, such that standardized stomatal sensitivity (<i>S</i>) decreased during the season. Despite lower D at the mid- and high-elevation sites, five diffuse-porous native species exhibited progressively increasing sensitivity to D during the growing season. 
4. Stomatal strategies exhibit seasonal trends that vary by environmental conditions (D) and functional traits (wood anatomy), which complicates the prediction of plant hydraulic function under increasing atmospheric drought.  In the increasingly arid western US, the progressively less sensitive stomatal behavior of invasive taxa may hasten their dominance in riparian forests. 

### Project organization

#### Data folders
Note that the data contain the site name 'Todd's', which was later changed to 'Parley's' in the manuscript to maintain consistency of site name reflecting the name of the body of water. 
- `raw-data/` contains the original .csv and .xlsx tabular data provided by Susan Bush and Kevin Hultine
  - `env_mesowest/` contains 15-minutely met data from the Olympus Hills station in 2004, accessed from [MesoWest](https://mesowest.utah.edu/)
  - `env_RBWS/` contains daily met data from stations 1, 2, 4, and 6 in Red Butte Canyon in 2004
  - `env_site/` contains daily met data from site-level met stations for the growing season of 2004. Each variable (mean, meax, and min of vapor pressure deficit (D) and air temperature (T)) was calculated both for the 24 hour period (daily) and for daylight hours (daytime) 
  - `Gc_30min/` contains 30-minutely data of canopy conductance calculated from sap flux density and D. Not used in this analysis
  - `Gc_daily`/ contains daytime canopy conductance calculated from mean daytime sap flux density and mean daytime D
  - `Js_daily/` contains mean daytime sap flux density
  - `waterpotential/` contains the original and compiled tabular records of predawn and midday leaf water potential
- `clean-data/` contains the cleaned .Rdata files processed from `raw-data/` with `scripts/manipulate/`
  - `env/` contains gapfilled values of maximum, mean, and cumulative D for all four sites in 2004
  - `sapflow/` contains daytime sapflux density and canopy conductance for all four sites in 2004
  - `waterpotential/` contains the raw, species means, and species by date means of predawn and midday leaf water potential

#### Script folders
- `scripts/` contains all R scripts for manipulating and plotting data
  - `manipulate/` gappfills, organizes, and cleans raw data into clean data
  -  `plot/` visualizes clean data and model output into figures for the manuscript and supplemental
- `model/` contains R and JAGS scripts for the hierarchical Bayesian model of time-varying stomatal sensitivity
  - `coda/` contains the posterior chains for the model parameters and replicated data
  - `diag/` contains the Gelman diagnostic for checking model convergence
  - `inits/` contains initial values for restarting the model at convergence
  - `output/` contains the preliminary model figures and calculated posterior output

#### Plot folder
- `plots/` contains the .jpg images at 600 dpi produced by `scripts/plot/`. All except 'Fig6_wp_muBeta.jpg' were included in the final manuscript and supplement
