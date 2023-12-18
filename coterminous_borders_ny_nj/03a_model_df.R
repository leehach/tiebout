library(tidyverse)
library(tableone)
library(pander)

# Primary analysis data frame `df_analysis` created in 01_data_handling.R.
# For simplicity, rename analysis frame to `df`. Retain only ID variables
# and variables that will be used in the models. Also allows us to continue
# working with `df` if we make incomplete changes to `df_analysis`.

df = df_analysis %>% select(
  county, suburban, bergen, nassau, queens, i_geoid, j_geoid, # Region, ID vars
  distance_km, distance_km2, # Spatial decay measures
  muni, township, # LG border variables
  sdelm, sdsec, sdelm_math_diff, sdelm_rla_diff, sdelm_comb_diff, # Education variables
  police, violent_diff, property_diff, z_violent_diff, z_property_diff, # Public safety variables
  mhi_diff, mhi_ln_diff, # Income variables
  black_diff, hispanic_diff, white_diff, asian_diff, children_diff # Demographic variables
  
)

df = df %>% mutate(
  muni = as.factor(muni), township = as.factor(township),
  sdelm = as.factor(sdelm), sdsec = as.factor(sdsec), police = as.factor(police),
  suburban = as.factor(suburban), bergen = as.factor(bergen), nassau = as.factor(nassau), queens = as.factor(queens),
  county = relevel(as.factor(county), ref = "Nassau")
)

# Output table of descriptive statistics
model_vars = c(
  "mhi_diff", "muni", 
  "black_diff", "hispanic_diff", "white_diff", "asian_diff", "children_diff", 
  "county", "distance_km", "distance_km2",
  "sdelm", "sdelm_rla_diff", "police", "violent_diff", "property_diff"
)
factor_vars = c("muni", "sdelm", "police", "county")

descr_stats = CreateTableOne(
  vars = model_vars,
  data = df,
  factorVars = factor_vars
)

pandoc.table(
  print(descr_stats, showAllLevels = FALSE),
  justify = "lr",
  style = "rmarkdown",
  split.table = Inf,
  emphasize.rownames = FALSE
)

