# Backup file with dummy variables as numerics

library(tidyverse)
library(modelsummary)

summary_stats = data.frame(
  raw = c("nobs", "adj.r.squared", "rmse"),
  clean = c("N", "Adj. R-squared", "RMSE"),
  fmt = c(0, 3, 3)
)

# Model 1a
# Model 1b
# Model 1c
# Model 1d
# Model 1e

# Base model
lm1a = lm(
  mhi_diff ~
    muni + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1a)

# Educational and police borders and quality
lm1b = lm(
  mhi_diff ~
    muni + sdelm + police +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1b)

lm1c = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + violent_diff + property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1c)

# Border Interaction Terms
lm1d = lm(
  mhi_diff ~
    muni + muni:bergen +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1d)

lm1e = lm(
  mhi_diff ~
    muni + muni:sdelm + muni:police + muni:sdelm:police +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1e)

lm1f = lm(
  mhi_diff ~
    muni + muni:sdelm_rla_diff + muni:property_diff + muni:sdelm_rla_diff:property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1f)

lm1g = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + children_diff:sdelm_rla_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1g)


lm1h = lm(
  mhi_diff ~
    muni + muni:white_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    bergen + queens + 
    distance_km + distance_km2, df
)
summary(lm1h)

models = list(
  "Model A" = lm1a,
  "Model B" = lm1b,
  "Model C" = lm1c,
  "Model D" = lm1d,
  "Model E" = lm1e,
  "Model F" = lm1f,
  "Model G" = lm1g,
  "Model H" = lm1h
)

modelsummary(
  models,
  output = "markdown",
  # estimate = "{estimate}{stars}",
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  gof_map = summary_stats,
  align = "ldddddddd"
)

modelsummary(
  models[1:4],
  output = "coterminous_borders_ny_nj/output/table1.md",
  # estimate = "{estimate}{stars}",
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  gof_map = summary_stats,
  align = "ldddd"
)

modelsummary(
  models[5:8],
  output = "coterminous_borders_ny_nj/output/table2.md",
  # estimate = "{estimate}{stars}",
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  gof_map = summary_stats,
  align = "ldddd"
)
