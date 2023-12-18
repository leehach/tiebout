library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmap)

census_vars = load_variables(2019, "acs5", cache = TRUE)

########################################
df_bergen_mcd = get_acs(
  geography = "county subdivision",
  variables = "B19013_001",
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = TRUE
)

df_bergen_mcd = st_transform(df_bergen_mcd, 26918)


tm_shape(df_bergen_mcd) +
  tm_polygons("B19013_001E")

###########################################
geo_nassau_mcd = get_acs(
  geography = "county subdivision",
  variables = "B19013_001",
  year = 2019,
  output = "wide",
  state = "NY",
  county = "Nassau",
  geometry = TRUE
)

geo_nassau_mcd = st_transform(geo_nassau_mcd, 26918)


tm_shape(geo_nassau_mcd) +
  tm_polygons("B19013_001E")


############################################

###########################################
geo_nys_place = get_acs(
  # geography = "place/remainder (or part)",
  geography = "place",
  variables = "B19013_001",
  year = 2019,
  output = "wide",
  state = "NY",
  # county = "Nassau",
  geometry = TRUE
)

geo_nassau_place = geo_nys_place %>%
#   filter(str_sub(GEOID, 3, 5) == "059") %>%
  st_transform(26918)


tm_shape(geo_nassau_place) +
  tm_polygons("B19013_001E")

# Doesn't work in tidycensus, does work in API call but have to supply county subdivision.
# Subminor civil division returns same result as place/remainder(part)
# Would need to separate villages from CDPs
# B19013_001 is Null for all entries
###########################################

tmp = block_groups(
  state = "NY",
  county = "Nassau",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918)

tm_shape(tmp) +
  tm_polygons()

###########################################################
vars10 <- c("P005003", "P005004", "P005006", "P004003")

il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "IL", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))

rm(vars10, il)

# Group Quarters
# NOT AVAILABLE AT TRACT LEVEL!!!
df_group_quarters_test = get_acs(
  geography = "tract",
  table = "B26103",
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = FALSE
)

##########################################################


test_new_pseudo_blkgrp = bind_rows(
  filter(gdf_nassau_blkgrp, geoid %in% use_blkgrps),
  filter(gdf_ny_place, geoid %in% use_places),
  filter(gdf_bergen_blkgrp, geoid %in% use_blkgrps),
  filter(gdf_nj_place, geoid %in% use_places)
)

not_in_new_blkgrp = gdf_pseudo_blkgrp %>%
  filter(!(geoid %in% test_new_pseudo_blkgrp$geoid))

# 
################## NEW JERSEY COUNTY SUBDIVISIONS (No longer needed)

split_blkgrps = unique(geocorr_blkgrp_cousub$blkgrp[duplicated(geocorr_blkgrp_cousub$blkgrp)])


# Filter:
#    Keep IDs of split block groups
#    Remove block groups where < 10% of the population is split (keep between 10% and 90%)
tmp = geocorr_blkgrp_cousub %>% 
  filter(blkgrp %in% split_blkgrps & afact >= 0.1 & afact < 0.90 )

use_cousub = unique(tmp$cousub)

# Now find all block groups not in county subdivisions in the `use_cousub` vector
# Exclude block groups with significant group quarters population
# Also remove block group parts with < 5% of block group population (retain 95% part)
tmp = geocorr_blkgrp_cousub %>%
  filter(
    afact >= 0.05 
    & !(cousub %in% use_cousub) 
    & !(blkgrp %in% gq_exclusion)
  )

use_blkgrps = tmp$blkgrp
rm(tmp)

# Check no duplicates
length(use_blkgrps[duplicated(use_blkgrps)]) == 0

# Extend gdf_pseudo_blkgrp geodataframe
gdf_pseudo_blkgrp = bind_rows(
  gdf_pseudo_blkgrp,
  filter(gdf_bergen_blkgrp, geoid %in% use_blkgrps),
  filter(gdf_nj_cousub, geoid %in% use_cousub)
)


# Quick view of results:
# tm_shape(gdf_pseudo_blkgrp) + tm_polygons()


#######################################################
# ADD QUEENS DATA
#######################################################
# `get_vars` already exists, has all desired demographic variables

# Get block groups
#gdf_queens_blkgrp = block_groups(
#  state = "NY",
#  county = "Queens",
#  cb = TRUE,
#  year = 2019
#) %>% st_transform(26918) %>% rename_with(tolower)
#

# Don't need to construct pseudo-block groups

# Add population centers to block groups
#df_queens_center_of_population = read_csv("coterminous_borders_ny_nj/data/CenPop2010_Mean_BG.csv") %>%
#  rename_with(tolower) %>%
#  filter(statefp == "36" & countyfp == "081") %>%
#  mutate(blkgrp = paste0(statefp, countyfp, tractce, blkgrpce))
#
#gdf_queens_center_of_population = df_queens_center_of_population %>%
#  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#  st_transform(26918) %>%
#  select(blkgrp) %>%
#  rename(geoid = blkgrp)

# Create network object
#row.names(gdf_queens_blkgrp) = gdf_queens_blkgrp$geoid # Necessary, as row.names parameter of poly2nb is ignored for sf objects
#nb_queens = poly2nb(gdf_queens_blkgrp, queen = FALSE)
#lw_queens = nb2listw(nb_queens, style = "B")
#
# gdf_cxn = listw2lines(lw_blkgrp, st_centroid(st_geometry(gdf_pseudo_blkgrp)), as_sf = TRUE)
# gdf_cxn = listw2lines(lw, st_point_on_surface(st_geometry(gdf_pseudo_blkgrp)), as_sf = TRUE)
#gdf_queens_cxn = listw2lines(lw_queens, st_geometry(gdf_queens_center_of_population), as_sf = TRUE)
#
# Remove duplicates:
#gdf_queens_cxn = gdf_queens_cxn %>% filter(i < j)


# Get block group data
#zz_df_queens_blkgrp = get_acs(
#  geography = "block group",
#  variables = get_vars,
#  year = 2019,
#  output = "wide",
#  state = "NY",
#  county = "Queens",
#  geometry = FALSE
#)

#df_queens_demographics = zz_df_queens_blkgrp %>%
#  select(-ends_with("M")) %>%
#  rename(
#    population = B01003_001E # Total Population
#    , white = B03002_003E # White NH
#    , black = B03002_004E # Black NH
#    , aian = B03002_005E # AIAN NH
#    , asian = B03002_006E # Asian NH
#    , hipi = B03002_007E # Hawaiian Pacific Islander NH
#    , other = B03002_008E # Other NH
#    , two_races = B03002_009E # Two or more NH
#    , hispanic = B03002_012E # Hispanic/Latino
#    , hh_all = B11005_001E # All households
#    , hh_children = B11005_002E # Households with children
#    , hh_no_children = B11005_011E # Households without children
#    , hh_0k = B19001_002E # Less than $10,000
#    , hh_10k = B19001_003E # $10,000 to $14,999
#    , hh_15k = B19001_004E # $15,000 to $19,999
#    , hh_20k = B19001_005E # $20,000 to $24,999
#    , hh_25k = B19001_006E # $25,000 to $29,999
#    , hh_30k = B19001_007E # $30,000 to $34,999
#    , hh_35k = B19001_008E # $35,000 to $39,999
#    , hh_40k = B19001_009E # $40,000 to $44,999
#    , hh_45k = B19001_010E # $45,000 to $49,999
#    , hh_50k = B19001_011E # $50,000 to $59,999
#    , hh_60k = B19001_012E # $60,000 to $74,999
#    , hh_75k = B19001_013E # $75,000 to $99,999
#    , hh_100k = B19001_014E # $100,000 to $124,999
#    , hh_125k = B19001_015E # $125,000 to $149,999
#    , hh_150k = B19001_016E # $150,000 to $199,999
#    , hh_200k = B19001_017E # $200,000 or more
#    , mhi = B19013_001E # Median Household Income
#    , agg_hh_inc = B19025_001E # Aggregate Household Income
#  ) %>%
#  rename_with(tolower)
  
# Calculate diffs between neighboring units
df_queens_demographics = df_queens_demographics %>% select(-name)

df_queens_analysis = gdf_queens_cxn %>%
  mutate(bergen = FALSE, nassau = FALSE, distance_m = st_length(geometry), distance_m2 = distance_m^2) %>%
  st_drop_geometry() %>%
  select(-i, -j, -wt) %>%
  rename(i_geoid = i_ID, j_geoid = j_ID)

df_queens_analysis = df_queens_analysis %>%
  inner_join(df_queens_demographics, by = c("i_geoid" = "geoid")) %>%
  inner_join(df_queens_demographics, by = c("j_geoid" = "geoid"), suffix = c("_i", "_j"))

# All of Queens is in same service unit
# df_analysis = df_analysis %>%
#   left_join(df_service_units, by = c("i_geoid" = "geoid")) %>%
#   left_join(df_service_units, by = c("j_geoid" = "geoid"), suffix = c("_i", "_j"))

# Difference in Median Household Income
df_queens_analysis = df_queens_analysis %>% mutate(
  mhi_diff = abs(mhi_i - mhi_j),
  mhi_ln_diff = abs(log(mhi_i) - log(mhi_j))
)

# # Index of Dissimilarity Black-White
# df_analysis = df_analysis %>% mutate(
#   d_white_black = 0.5 * (abs((white_i / (white_i + white_j)) - (black_i / (black_i + black_j))) + 
#                            abs((white_j / (white_i + white_j)) - (black_j / (black_i + black_j))))
# )

# Classify by service area changes
df_queens_analysis = df_queens_analysis %>% mutate(
  muni = FALSE,
  township = FALSE,
  sdelm = FALSE,
  sdsec = FALSE
  # police = police_i == police_j # All pseudo-BGs should be assigned toa  police agency # NOT READY
)

################################################
# Nassau county number of split school districts
fn = "coterminous_borders_ny_nj/data/geocorr2014_place_sdbest.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_place_sdbest = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_place_sdbest = geocorr_place_sdbest %>% mutate(
  place = paste0(state, placefp14),
  sduni = if_else(is.na(sduni14), NA, paste0(state, sduni14))
)

df_village_sdbest = geocorr_place_sdbest %>%
  filter(str_detect(placenm14, "village, NY"))

df_village_sdbest %>% filter(afact2 == 1)
# One village has a coterminous school district (Garden City)

df_village_sdbest %>%
  filter(afact2 != 1) %>%
  count(placenm14) %>%
  mutate(single_sd = n==1) %>%
  count(single_sd)
# 33 villages fall within a single school district, 30 are split across school districts

bergen_sdelm_ids = unique(df_blkgrp_service_units$sdelm[substr(df_blkgrp_service_units$blkgrp, 1, 5) == "34003"])
nassau_sdelm_ids = unique(df_blkgrp_service_units$sdelm[substr(df_blkgrp_service_units$blkgrp, 1, 5) == "36059"])

sdelm_bergen_quality = df_sdelm_quality$sdelm_rla[df_sdelm_quality$sedalea %in% bergen_sdelm_ids]
sdelm_nassau_quality = df_sdelm_quality$sdelm_rla[df_sdelm_quality$sedalea %in% nassau_sdelm_ids]

mean(sdelm_bergen_quality)
mean(sdelm_nassau_quality)

sd(sdelm_bergen_quality)
sd(sdelm_nassau_quality)

# Bergen and Nassau Demographics
get_vars = c(
  "B01003_001" # Total Population
  , "B03002_003" # White NH
  , "B03002_004" # Black NH
  , "B03002_005" # AIAN NH
  , "B03002_006" # Asian NH
  , "B03002_007" # Hawaiian Pacific Islander NH
  , "B03002_008" # Other NH
  , "B03002_009" # Two or more NH
  , "B03002_012" # Hispanic/Latino
  , "B11005_001" # All households
  , "B11005_002" # Households with children
  , "B11005_011" # Households without children
  # , "B19001_001" # All households ### Same as B11005_001
  , "B19001_002" # Less than $10,000
  , "B19001_003" # $10,000 to $14,999
  , "B19001_004" # $15,000 to $19,999
  , "B19001_005" # $20,000 to $24,999
  , "B19001_006" # $25,000 to $29,999
  , "B19001_007" # $30,000 to $34,999
  , "B19001_008" # $35,000 to $39,999
  , "B19001_009" # $40,000 to $44,999
  , "B19001_010" # $45,000 to $49,999
  , "B19001_011" # $50,000 to $59,999
  , "B19001_012" # $60,000 to $74,999
  , "B19001_013" # $75,000 to $99,999
  , "B19001_014" # $100,000 to $124,999
  , "B19001_015" # $125,000 to $149,999
  , "B19001_016" # $150,000 to $199,999
  , "B19001_017" # $200,000 or more
  , "B19013_001" # Median Household Income
  , "B19025_001" # Aggregate Household Income
  # , "B26001_001" # Group Quarters Population (N/A for block groups; need to use from tracts to exclude some block groups)
  # , "B26101_214" # Household Population
)

# Get block group data
zz_df_counties = get_acs(
  geography = "county",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = c("NJ", "NY"),
  geometry = FALSE
)

df_counties = zz_df_counties %>%
  st_drop_geometry() %>%
  select(-ends_with("M")) %>%
  rename(
    population = B01003_001E # Total Population
    , white = B03002_003E # White NH
    , black = B03002_004E # Black NH
    , aian = B03002_005E # AIAN NH
    , asian = B03002_006E # Asian NH
    , hipi = B03002_007E # Hawaiian Pacific Islander NH
    , other = B03002_008E # Other NH
    , two_races = B03002_009E # Two or more NH
    , hispanic = B03002_012E # Hispanic/Latino
    , hh_all = B11005_001E # All households
    , hh_children = B11005_002E # Households with children
    , hh_no_children = B11005_011E # Households without children
    , hh_5k = B19001_002E # Less than $10,000
    , hh_12.5k = B19001_003E # $10,000 to $14,999
    , hh_17.5k = B19001_004E # $15,000 to $19,999
    , hh_22.5k = B19001_005E # $20,000 to $24,999
    , hh_27.5k = B19001_006E # $25,000 to $29,999
    , hh_32.5k = B19001_007E # $30,000 to $34,999
    , hh_37.5k = B19001_008E # $35,000 to $39,999
    , hh_42.5k = B19001_009E # $40,000 to $44,999
    , hh_47.5k = B19001_010E # $45,000 to $49,999
    , hh_55k = B19001_011E # $50,000 to $59,999
    , hh_67.5k = B19001_012E # $60,000 to $74,999
    , hh_87.5k = B19001_013E # $75,000 to $99,999
    , hh_112.5k = B19001_014E # $100,000 to $124,999
    , hh_137.5k = B19001_015E # $125,000 to $149,999
    , hh_175k = B19001_016E # $150,000 to $199,999
    , hh_250k = B19001_017E # $200,000 or more
    , mhi = B19013_001E # Median Household Income
    , agg_hh_inc = B19025_001E # Aggregate Household Income
  ) %>%
  rename_with(tolower)

df_counties_race = df_counties %>%
  mutate(
    geoid, name,
    pct_white = 100 * white/population,
    pct_black = 100 * black / population,
    pct_hispanic = 100 * hispanic / population,
    pct_asian = 100 * asian / population,
    .keep = "used"
  )

# Nassau population density per sqkm
df_counties$population[df_counties$geoid == "36059"] / (gdf_nassau$aland / 1000000)

# Bergen population density per sqkm
df_counties$population[df_counties$geoid == "34003"] / (gdf_bergen$aland / 1000000)

library(dplyr)
library(magrittr)

geoid = seq(100)
df_example = data.frame(
  geoid = geoid,
  inc1 = sample(0:200, 100),
  inc2 = sample(0:200, 100),
  inc3 = sample(0:200, 100),
  inc4 = sample(0:200, 100),
  inc5 = sample(0:200, 100)
)

df_example$total = df_example$inc1 + df_example$inc2 + df_example$inc3 + df_example$inc4 + df_example$inc5

df_example = df_example %>% mutate(
  inc1_cumsum = inc1,
  inc2_cumsum = inc1 + inc2,
  inc3_cumsum = inc1 + inc2 + inc3,
  inc4_cumsum = inc1 + inc2 + inc3 + inc4,
  inc5_cumsum = inc1 + inc2 + inc3 + inc4 + inc5
)

df_example = df_example %>% mutate(
  median_inc_bin = case_when(
    total/2 < inc1_cumsum ~ "inc1",
    total/2 < inc2_cumsum ~ "inc2",
    total/2 < inc3_cumsum ~ "inc3",
    total/2 < inc4_cumsum ~ "inc4",
    total/2 < inc5_cumsum ~ "inc5"
  )
)

df_example %>%
  pivot_longer(-geoid) %>%
  filter(sum(value)/2 < cumsum(value), .by = geoid) %>%
  slice_head(n = 1, by = geoid)