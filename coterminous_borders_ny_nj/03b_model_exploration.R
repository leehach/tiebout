library(tidyverse)
library(ggeffects)
library(see)
library(parameters)

summary(lm(mhi_diff ~  bergen + nassau + muni + sdsec + sdelm_rla_diff + distance_m + distance_m2, df))
summary(lm(mhi_diff ~  bergen + nassau + muni + sdelm_rla_diff + black_diff + hispanic_diff + children_diff + distance_m + distance_m2, df))
summary(lm(mhi_diff ~  muni + sdelm_rla_diff + distance_m + distance_m2, df))

summary(lm(mhi_diff ~  
             bergen + queens + muni + 
             # sdelm +
             # sdelm_comb_diff + 
             sdelm_rla_diff + 
             # sdelm_math_diff + 
             violent_diff + property_diff + 
             # z_violent_diff + z_property_diff + 
             black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
             distance_m + distance_m2, df
           ))

summary(lm(mhi_diff ~  
             bergen + queens + muni * sdelm_rla_diff * violent_diff + 
             # bergen + queens + muni * sdelm_rla_diff * violent_diff * property_diff + 
             # sdelm +
             # sdelm_comb_diff + 
             # sdelm_math_diff + 
             # z_violent_diff + z_property_diff + 
             black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
             distance_m + distance_m2, df
))

summary(lm(mhi_diff ~  
             bergen + queens + muni*white_diff + 
             sdelm_rla_diff + sdelm_math_diff + 
             violent_diff + property_diff + 
             black_diff + hispanic_diff + asian_diff + children_diff + 
             distance_m + distance_m2, df
))

summary(lm(mhi_diff ~  
             # queens + 
             bergen*muni -bergen + white_diff +
             sdelm_rla_diff + sdelm_math_diff + 
             violent_diff + property_diff + 
             black_diff + hispanic_diff + asian_diff + children_diff + 
             distance_m + distance_m2, df
))

lm_mhi0 = lm(mhi_diff ~ bergen + queens + muni + sdelm_rla_diff + distance_m + distance_m2, df)
lm_mhi5 = lm(mhi_diff ~ bergen + queens + muni + sdelm_rla_diff + children_diff + distance_m + distance_m2, df)
summary(lm_mhi5)
plot(lm_mhi5)

anova(lm_mhi1, lm_mhi5, test = "F")

summary(
  lm(
    mhi_diff ~
      muni + muni:sdelm + muni:police + muni:sdelm:police +
      black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
      queens + 
      distance_km + distance_km2, df
  )
)

summary(
  lm(
    mhi_diff ~
      muni + muni:sdelm_rla_diff + muni:property_diff + muni:sdelm_rla_diff:property_diff +
      black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
      queens + 
      distance_km + distance_km2, df
  )
)

######################################################################################
# Interaction Effects
######################################################################################

# df_explore = df %>% mutate(county = relevel(county, ref = "Queens"))

# Base model
test_lm1a = lm(
  mhi_diff ~
    muni + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1a)
model_parameters(test_lm1a)
hypothesis_test(test_lm1a, "county")
pred_test_lm1a = ggpredict(test_lm1a, c("muni", "county"))
plot(pred_test_lm1a)
## No interaction, slope is the same for all counties

# Educational and police borders
test_lm1b = lm(
  mhi_diff ~
    muni + sdelm + police +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1b)
model_parameters(test_lm1b)
hypothesis_test(test_lm1b, "county")
pred_test_lm1b = ggpredict(test_lm1b, c("county[Nassau, Bergen]", "muni"))
plot(pred_test_lm1b)
## No interaction, slope is the same for all counties

# Educational and police quality
test_lm1c = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + violent_diff + property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1c)
model_parameters(test_lm1c)
hypothesis_test(test_lm1c, "county")
pred_test_lm1c = ggpredict(test_lm1c, c("sdelm_rla_diff", "muni", "county"))
plot(ggpredict(test_lm1c, c("sdelm_rla_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1c, c("violent_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1c, c("property_diff", "muni", "county[Nassau, Bergen]")))

## No interaction, slope is the same for all counties

# Border Interaction Terms
test_lm1d = lm(
  mhi_diff ~
    muni * county + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    distance_km + distance_km2, df
)
summary(test_lm1d)
model_parameters(test_lm1d)
hypothesis_test(test_lm1d, c("muni", "county[Nassau, Bergen]"))
pred_test_lm1d = ggpredict(test_lm1d, c("muni", "county[Nassau, Bergen]")) # ggpredict(test_lm1d, c("muni", "county"))
pred_test_lm1d = ggpredict(test_lm1d, c("county[Nassau, Bergen]", "muni"))
plot(pred_test_lm1d)

test_lm1e = lm(
  mhi_diff ~
    muni * sdelm * police + 
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1e)
model_parameters(test_lm1e)
hypothesis_test(test_lm1e, c("muni", "county[Nassau, Bergen]"))
hypothesis_test(test_lm1e, c("sdelm", "police", "muni"))
hypothesis_test(test_lm1e, c("police", "muni"))
pred_test_lm1e = ggpredict(test_lm1e, c("muni", "sdelm", "police", "county[Nassau, Bergen]"))
plot(pred_test_lm1e)
plot(ggpredict(test_lm1e, c("muni", "sdelm", "county[Nassau, Bergen]", "police")))
plot(ggpredict(test_lm1e, c("sdelm", "police", "muni")))
plot(ggpredict(test_lm1e, c("police", "sdelm", "muni")), connect.lines = TRUE)

test_lm1f = lm(
  mhi_diff ~
    muni * sdelm_rla_diff * property_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1f)
model_parameters(test_lm1f)
hypothesis_test(test_lm1f, c("muni", "county[Nassau, Bergen]"))
plot(ggpredict(test_lm1f, c("sdelm_rla_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1f, c("property_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1f, c("sdelm_rla_diff", "property_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1f, c("sdelm_rla_diff", "property_diff", "muni")))
hypothesis_test(test_lm1f, c("sdelm_rla_diff", "property_diff", "muni"), test = NULL)
hypothesis_test(test_lm1f, c("sdelm_rla_diff", "property_diff", "muni"))
plot(ggpredict(test_lm1f, c("property_diff", "sdelm_rla_diff", "muni")))
hypothesis_test(test_lm1f, c("property_diff", "sdelm_rla_diff", "muni"))

# Children and school district quality interaction
test_lm1g = lm(
  mhi_diff ~
    muni + sdelm_rla_diff + children_diff:sdelm_rla_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1g)
model_parameters(test_lm1g)
hypothesis_test(test_lm1g, c("muni", "county[Nassau, Bergen]"))
pred_test_lm1g = ggpredict(test_lm1g, c("muni", "sdelm_rla_diff", "police", "county[Nassau, Bergen]"))
# plot(pred_test_lm1e)
plot(ggpredict(test_lm1g, c("sdelm_rla_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1g, c("sdelm_rla_diff", "children_diff", "muni")))

# Municipal border and white households interaction
test_lm1h = lm(
  mhi_diff ~
    muni + muni:white_diff +
    black_diff + hispanic_diff + white_diff + asian_diff + children_diff + 
    county + 
    distance_km + distance_km2, df
)
summary(test_lm1h)
model_parameters(test_lm1h)
hypothesis_test(test_lm1h, c("muni", "county[Nassau, Bergen]"))
pred_test_lm1h = ggpredict(test_lm1h, c("muni", "white_diff", "county[Nassau, Bergen]"))
plot(pred_test_lm1h)
plot(ggpredict(test_lm1h, c("white_diff", "muni", "county[Nassau, Bergen]")))
plot(ggpredict(test_lm1h, c("white_diff", "muni")))
