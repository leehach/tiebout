library(tidyverse)
library(modelsummary)
library(ggeffects)

summary_stats = data.frame(
  raw = c("nobs", "adj.r.squared", "rmse"),
  clean = c("N", "Adj. R-squared", "RMSE"),
  fmt = c(0, 3, 3)
)

# Base model
lm1a = lm(
  mhi_diff ~
    muni + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1a)

# Educational and police borders
lm1b = lm(
  mhi_diff ~
    muni + sdelm + police +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1b)

# Educational and police quality
lm1c = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + violent_diff + property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1c)

# Border Interaction Terms
lm1d = lm(
  mhi_diff ~
    muni * county + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    distance_km + distance_km2, df
)
summary(lm1d)

lm1e = lm(
  mhi_diff ~
    muni * sdelm * police + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1e)
hypothesis_test(lm1e, c("sdelm", "police", "muni"), test = NULL)
hypothesis_test(lm1e, c("sdelm", "police", "muni"))

lm1f = lm(
  mhi_diff ~
    muni * sdelm_rla_diff * property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1f)
hypothesis_test(lm1f, c("sdelm_rla_diff", "property_diff", "muni"), test = NULL)
hypothesis_test(lm1f, c("sdelm_rla_diff", "property_diff", "muni"))
hypothesis_test(lm1f, c("sdelm_rla_diff", "muni"))
hypothesis_test(lm1f, c("property_diff", "muni"))

# Children and school district quality interaction
lm1g = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + children_diff:sdelm_rla_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1g)
hypothesis_test(lm1g, c("children_diff", "sdelm_rla_diff"))

# Municipal border and white households interaction
lm1h = lm(
  mhi_diff ~
    muni + muni:white_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(lm1h)
hypothesis_test(lm1h, c("white_diff", "muni"))

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
  output = "coterminous_borders_ny_nj/output/table2.md",
  # estimate = "{estimate}{stars}",
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  gof_map = summary_stats,
  align = "ldddd"
)

modelsummary(
  models[5:8],
  output = "coterminous_borders_ny_nj/output/table3.md",
  # estimate = "{estimate}{stars}",
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  gof_map = summary_stats,
  align = "ldddd"
)
