# No objects created in this script should be reused in a later script

library(tmap)

# tm_shape(gdf_bergen_tract) +
#   tm_polygons("B19013_001E") +
# tm_shape(sf_bergen_tract_pairs) +
#   tm_lines()

tm_shape(gdf_bergen_blkgrp) +
  tm_polygons("B19013_001E") +
  
# Plot tract connections
plot(st_geometry(gdf_bergen_tract))
plot(nb_bergen_tract, gdf_bergen_coords, add = TRUE)

plot(nb_bergen_tract, gdf_bergen_coords)

tm_shape(gdf_blkgrp_cxn) +
  tm_lines()

tm_shape(gdf_ny_place) +
  tm_polygons()

tm_shape(gdf_blkgrp) +
  tm_polygons()

tm_shape(gdf_nj_place) +
  tm_polygons() +
  tm_shape(gdf_bergen) +
  tm_polygons(col = "red")

tm_shape(gdf_bergen_blkgrp) +
  tm_polygons(lwd = 0.1) +
tm_shape(filter(gdf, str_sub(i_ID, 1, 2) == "34")) +
  tm_lines("mhi_diff", lwd = "mhi_diff", scale = 5, palette = "YlOrRd")

tm_shape(gdf_pseudo_blkgrp) +
  tm_polygons()

# Following block group IDs overlap South Hackensack Township:
south_hackensack_blkgrps = c(340030600001, 340030361001, 340030362001)
geocorr_blkgrp_place %>% 
  filter(blkgrp %in% south_hackensack_blkgrps)
# # A tibble: 5 × 17
#   county tract   bg    state placefp14 stab  cntyname  placefp placenm placenm14              intptlon intptlat pop10  afact   afact2 blkgrp       place  
#   <chr>  <chr>   <chr> <chr> <chr>     <chr> <chr>     <chr>   <chr>   <chr>                     <dbl>    <dbl> <int>  <dbl>    <dbl> <chr>        <chr>  
# 1 34003  0361.00 1     34    72480     NJ    Bergen NJ NA      NA      Teterboro borough, NJ     -74.1     40.9    67 0.0622 1        340030361001 3472480
# 2 34003  0361.00 1     34    99999     NJ    Bergen NJ NA      NA      NA                        -74.0     40.9  1010 0.938  0.00705  340030361001 3499999
# 3 34003  0362.00 1     34    47700     NJ    Bergen NJ NA      NA      Moonachie borough, NJ     -74.0     40.8  1779 1      0.657    340030362001 3447700
# 4 34003  0600.00 1     34    82570     NJ    Bergen NJ NA      NA      Wood-Ridge borough, NJ    -74.1     40.9   782 0.92   0.103    340030600001 3482570
# 5 34003  0600.00 1     34    99999     NJ    Bergen NJ NA      NA      NA                        -74.1     40.9    68 0.08   0.000475 340030600001 3499999

# Race/ethnicity
df_blkgrp %>%
  select(geoid, white, black, aian, asian, hipi, other, two_races, hispanic) %>%
  pivot_longer(-geoid, names_to = "race", values_to = "count") %>%
  ggplot(aes(x = race, y = count)) +
  geom_boxplot()

ggplot(df_blkgrp, aes(aian)) +
  geom_boxplot()
df_blkgrp %>% filter(aian > 100)
df_blkgrp %>% filter(aian > 0) %>% nrow()
# [1] 134

ggplot(df_blkgrp, aes(hipi)) +
  geom_boxplot()
df_blkgrp %>% filter(hipi > 40)
df_blkgrp %>% filter(hipi > 0) %>% nrow()
# [1] 32

ggplot(df_blkgrp, aes(other)) +
  geom_boxplot()
df_blkgrp %>% filter(other > 0) %>% nrow()
# [1] 266

ggplot(df_blkgrp, aes(two_races)) +
  geom_boxplot()
df_blkgrp %>% filter(two_races > 200)
df_blkgrp %>% filter(two_races > 0) %>% nrow()
# [1] 966

# This is repeated in 03_analysis. Should be the same.
df = df_analysis %>% select(
  bergen, nassau, i_geoid, j_geoid, # Region, ID vars
  distance_m, distance_m2, # Spatial decay measures
  muni, township, # LG border variables
  sdelm, sdsec, sdelm_math_diff, sdelm_rla_diff, # Education variables
  police, violent_crime_diff, property_crime_diff, # Public safety variables
  mhi_diff, mhi_ln_diff, # Income variables
  black_diff, hispanic_diff, children_diff # Demographic variables
  
)
df$queens = !(df$bergen | df$nassau)

ggplot(df, aes(x = mhi_diff, fill = muni)) +
  geom_density(alpha = 0.4)

ggplot(df, aes(x = mhi_diff, fill = muni)) +
  geom_histogram(alpha = 0.4)

ggplot(df, aes(x = muni, y= mhi_diff)) +
  geom_boxplot()

ggplot(df, aes(x = mhi_ln_diff, fill = muni)) +
  geom_density(alpha = 0.4)

ggplot(df, aes(x = mhi_ln_diff, fill = muni)) +
  geom_histogram(alpha = 0.4)

ggplot(df, aes(x = muni, y= mhi_ln_diff)) +
  geom_boxplot()

ggplot(df, aes(sample = mhi_diff)) +
  stat_qq() + stat_qq_line()
shapiro.test(sample(df$mhi_diff, 5000))

ggplot(df, aes(sample = mhi_ln_diff)) +
  stat_qq() + stat_qq_line()
shapiro.test(sample(df$mhi_ln_diff, 5000))

cor.test(df$sdelm_math_diff, df$sdelm_rla_diff)

# Pearson's product-moment correlation
# 
# data:  df$sdelm_math_diff and df$sdelm_rla_diff
# t = 318.41, df = 9362, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9550531 0.9584789
# sample estimates:
#       cor 
# 0.9567992 

# Queens police precincts
qtm(gdf_nypd_precincts)
tm_shape(gdf_queens_blkgrp_police) +
  tm_polygons("police")

# Bergen police agencies
qtm(gdf_bergen_blkgrp)
tm_shape(inner_join(gdf_bergen_blkgrp, df_bergen_blkgrp_police, by = join_by(geoid == blkgrp))) +
  tm_polygons("police")

# Nassau police agencies
qtm(gdf_nassau_police_districts)
tm_shape(gdf_nassau_blkgrp_police) +
  tm_polygons("police")
