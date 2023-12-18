library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(spdep)

# 1. Import data by Census tract
# 2. Pair Census tracts (spdep)
# 3. Compute pairwise statistics
#   a. Difference in median household income
#   b. t-statistic of incomes
#   c. Theil measure of income inequality?
#   d. D Black-white
#   e. D Top-two groups
#   f. Multi-group segregation measure?
#   g. D presence of children?
#   h. Difference in secondary school quality
#   i. Difference in violent crime rate
#   j. Difference in property crime rate
# 
# 

# Use EPSG:26918 = UTM18N for all geometries

# Data sources:
# * Nassau small PD crime statistics: https://data.ny.gov/Public-Safety/Index-Crimes-by-County-and-Agency-Beginning-1990/ca8h-8gjq
# 
# Geographies:
# For Bergen county, use county subdivisions.
#   T1 = An active county subdivision that is not coextensive with an incorporated place
#   C5 = An active incorporated place that is independent of any county subdivision and serves as a county subdivision equivalent
# For Nassau county, have to use both places and county subdivisions.
#   C1 	An active incorporated place that does not serve as a county subdivision equivalent 	Economic Census Place, Incorporated Place
#   C5 	An active incorporated place that is independent of any county subdivision and serves as a county subdivision equivalent 	County Subdivision, Economic Census Place, Incorporated Place
#   U1 	A census designated place with an official federally recognized name 	Census Designated Place, Economic Census Place
#   U2 	A census designated place without an official federally recognized name 	Census Designated Place, Economic Census Place
#   M1 	A military or other defense installation that is not entirely within a census designated place 	Locality Point, Military Installation 

# CRIME DATA
# Offenses have different names in different sources. Use following standard names (aggregating up to violent and property crimes)
# - violent_crime
#   - murder
#   - rape
#   - robbery
#   - assault
# - property_crime
#   - burglary
#   - larceny
#   - motor_vehicle_theft

##########################################
# GET GEOMETRIES
##########################################
# Transform all geometries to UTM18N, appropriate for both NY and NJ counties in NYC metro area

# Get block groups
gdf_bergen_blkgrp = block_groups(
  state = "NJ",
  county = "Bergen",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918) %>% rename_with(tolower)

gdf_nassau_blkgrp = block_groups(
  state = "NY",
  county = "Nassau",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918) %>% rename_with(tolower)

gdf_queens_blkgrp = block_groups(
  state = "NY",
  county = "Queens",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918) %>% rename_with(tolower)
# TODO: Need to exclude industrial areas, airports, group quarters, etc.!!!

# Get Nassau County places

gdf_nassau = counties(
  state = "NY",
  cb = TRUE,
  year = 2019
) %>% 
  filter(COUNTYFP == "059") %>%
  st_transform(26918) %>% rename_with(tolower)

gdf_ny_place = places(
  state = "NY",
  cb = TRUE,
  year = 2019,
  filter_by = gdf_nassau
) %>% st_transform(26918) %>%
  rename_with(tolower)
tmp = gdf_ny_place %>%
  st_point_on_surface() %>%
  st_filter(gdf_nassau) %>%
  st_drop_geometry()
tmp = tmp$geoid
gdf_ny_place = gdf_ny_place %>%
  filter(geoid %in% tmp)

# Need to add govt CLASS
tmp = places(
  state = "NY",
  cb = FALSE,
  year = 2019
) %>% st_drop_geometry() %>% rename_with(tolower)

gdf_ny_place = inner_join(
  gdf_ny_place, select(tmp, geoid, classfp), by = join_by(geoid)
)
rm(tmp)

df_ny_place_class = gdf_ny_place %>% 
  st_drop_geometry() %>%
  select(geoid, classfp) %>%
  mutate(geoid2 = geoid) %>%
  pivot_wider(
    id_cols = geoid,
    names_from = classfp,
    names_sort = TRUE,
    values_from = geoid2
  ) %>%
  rename_with(tolower)

# Get Bergen County places
gdf_bergen = counties(
  state = "NJ",
  cb = TRUE,
  year = 2019
) %>% 
  filter(COUNTYFP == "003") %>%
  st_transform(26918) %>% rename_with(tolower)

gdf_nj_place = places(
  state = "NJ",
  cb = TRUE,
  year = 2019,
  filter_by = gdf_bergen
) %>% st_transform(26918) %>%
  rename_with(tolower)
tmp = gdf_nj_place %>%
  st_point_on_surface() %>%
  st_filter(gdf_bergen) %>%
  st_drop_geometry()
tmp = tmp$geoid
gdf_nj_place = gdf_nj_place %>%
  filter(geoid %in% tmp)

# Need to add govt CLASS
tmp = places(
  state = "NJ",
  cb = FALSE,
  year = 2019
) %>% st_drop_geometry() %>% rename_with(tolower)

gdf_nj_place = inner_join(
  gdf_nj_place, select(tmp, geoid, classfp), by = join_by(geoid)
)
rm(tmp)

df_nj_place_class = gdf_nj_place %>% 
  st_drop_geometry() %>%
  select(geoid, classfp) %>%
  mutate(geoid2 = geoid) %>%
  pivot_wider(
    id_cols = geoid,
    names_from = classfp,
    names_sort = TRUE,
    values_from = geoid2
  ) %>%
  rename_with(tolower)

# Combine place-class lookup
df_place_class = bind_rows(df_ny_place_class, df_nj_place_class)

# Get Nassau County subdivisions
gdf_ny_cousub = county_subdivisions(
  state = "NY",
  county = "Nassau",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918) %>% rename_with(tolower)

tmp = county_subdivisions(
  state = "NY",
  county = "Nassau",
  cb = FALSE,
  year = 2019
) %>% st_drop_geometry() %>% rename_with(tolower)

gdf_ny_cousub = inner_join(
  gdf_ny_cousub, select(tmp, geoid, classfp), by = join_by(geoid)
)
rm(tmp)

df_ny_cousub_class = gdf_ny_cousub %>% 
  st_drop_geometry() %>%
  select(geoid, classfp) %>%
  mutate(geoid2 = geoid) %>%
  pivot_wider(
    id_cols = geoid,
    names_from = classfp,
    names_sort = TRUE,
    values_from = geoid2
  ) %>%
  rename_with(tolower)

# Get Bergen County subdivisions
gdf_nj_cousub = county_subdivisions(
  state = "NJ",
  county = "Bergen",
  cb = TRUE,
  year = 2019
) %>% st_transform(26918) %>% rename_with(tolower)

tmp = county_subdivisions(
  state = "NJ",
  county = "Bergen",
  cb = FALSE,
  year = 2019
) %>% st_drop_geometry() %>% rename_with(tolower)

gdf_nj_cousub = inner_join(
  gdf_nj_cousub, select(tmp, geoid, classfp), by = join_by(geoid)
)
rm(tmp)

df_nj_cousub_class = gdf_nj_cousub %>% 
  st_drop_geometry() %>%
  select(geoid, classfp) %>%
  mutate(geoid2 = geoid) %>%
  pivot_wider(
    id_cols = geoid,
    names_from = classfp,
    names_sort = TRUE,
    values_from = geoid2
  ) %>%
  rename_with(tolower)

# Combine cousub-class lookup
# Drop C5 class, which is represented at the place level
df_cousub_class = bind_rows(df_ny_cousub_class, df_nj_cousub_class) %>%
  select(geoid, t1)



#####################################################
# ASSIGN SERVICE UNIT IDs
#####################################################
# All correlation files come from Geocorr2014: 
# https://mcdc.missouri.edu/applications/geocorr2014.html

# Assign service unit IDs to all block groups:
# Municipalities (places)
# County subdivisions
# School districts: Elementary? Secondary? Both?
# Police districts

df_blkgrp_service_units = bind_rows(
  select(st_drop_geometry(gdf_bergen_blkgrp), blkgrp = geoid),
  select(st_drop_geometry(gdf_nassau_blkgrp), blkgrp = geoid)
)

df_queens_service_units = select(st_drop_geometry(gdf_queens_blkgrp), blkgrp = geoid)

# ASSIGN MUNICIPALITIES (PLACES)
fn = "coterminous_borders_ny_nj/data/geocorr2014_blkgrp_place.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccccddidd"
geocorr_blkgrp_place = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_blkgrp_place = geocorr_blkgrp_place %>% mutate(
  blkgrp = paste0(county, str_remove(tract, fixed(".")), bg),
  place = paste0(state, placefp14)
)

tmp = geocorr_blkgrp_place %>%
  select(blkgrp, place, share = afact) %>%
  group_by(blkgrp) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share) %>%
  inner_join(df_place_class, by = join_by(place == geoid)) %>%
  select(-place)

df_blkgrp_service_units = df_blkgrp_service_units %>%
  left_join(tmp, by = join_by(blkgrp))
df_queens_service_units = df_queens_service_units %>%
  mutate(c1 = "3651000", c5 = NA, u1 = NA, u2 = NA)
# Queens block groups are all part of NYC, place = 3651000

rm(fn, col_names, col_types)
# Retain geocorr_blkgrp_place for later use

# ASSIGN COUNTY SUBDIVISIONS
fn = "coterminous_borders_ny_nj/data/geocorr2014_blkgrp_cousub.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccddidd"
geocorr_blkgrp_cousub = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_blkgrp_cousub = geocorr_blkgrp_cousub %>% mutate(
  blkgrp = paste0(county, str_remove(tract, fixed(".")), bg),
  cousub = paste0(county, cousubfp14)
)

tmp = geocorr_blkgrp_cousub %>%
  select(blkgrp, cousub, share = afact) %>%
  group_by(blkgrp) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share) %>%
  inner_join(df_cousub_class, by = join_by(cousub == geoid)) %>%
  select(-cousub)

df_blkgrp_service_units = df_blkgrp_service_units %>%
  left_join(tmp, by = join_by(blkgrp))
df_queens_service_units = df_queens_service_units %>%
  mutate(t1 = "3608160323")
# Queens County has no subdivisions, officially coded as 3608160323

rm(fn, col_names, col_types, tmp)
rm(geocorr_blkgrp_cousub)

# ASSIGN SCHOOL DISTRICTS

fn = "coterminous_borders_ny_nj/data/geocorr2014_blkgrp_sduni.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_blkgrp_sduni = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_blkgrp_sduni = geocorr_blkgrp_sduni %>% mutate(
  blkgrp = paste0(county, str_remove(tract, fixed(".")), bg),
  sduni = if_else(is.na(sduni14), NA, paste0(state, sduni14))
)

fn = "coterminous_borders_ny_nj/data/geocorr2014_blkgrp_sdelm.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_blkgrp_sdelm = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_blkgrp_sdelm = geocorr_blkgrp_sdelm %>% mutate(
  blkgrp = paste0(county, str_remove(tract, fixed(".")), bg),
  sdelm = if_else(is.na(sdelm14), NA, paste0(state, sdelm14))
)

fn = "coterminous_borders_ny_nj/data/geocorr2014_blkgrp_sdsec.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_blkgrp_sdsec = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_blkgrp_sdsec = geocorr_blkgrp_sdsec %>% mutate(
  blkgrp = paste0(county, str_remove(tract, fixed(".")), bg),
  sdsec = if_else(is.na(sdsec14), NA, paste0(state, sdsec14))
)


df_blkgrp_sduni = geocorr_blkgrp_sduni %>%
  select(blkgrp, sduni, share = afact) %>%
  group_by(blkgrp) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)
df_blkgrp_sdelm = geocorr_blkgrp_sdelm %>%
  select(blkgrp, sdelm, share = afact) %>%
  group_by(blkgrp) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)
df_blkgrp_sdsec = geocorr_blkgrp_sdsec %>%
  select(blkgrp, sdsec, share = afact) %>%
  group_by(blkgrp) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)

tmp = df_blkgrp_sduni %>%
  inner_join(df_blkgrp_sdelm, by = join_by(blkgrp)) %>%
  inner_join(df_blkgrp_sdsec, by = join_by(blkgrp)) %>%
  mutate(sdelm = coalesce(sduni, sdelm), sdsec = coalesce(sduni, sdsec)) %>%
  select(-sduni)

df_blkgrp_service_units = df_blkgrp_service_units %>%
  left_join(tmp, by = join_by(blkgrp))
df_queens_service_units = df_queens_service_units %>%
  mutate(sdelm = "3620580", sdsec  = "3620580")
# For future, might want to add elementary school zones for Queens
#   - Only if can obtain school quality statistics

rm(fn, col_names, col_types, tmp)
rm(geocorr_blkgrp_sdelm, geocorr_blkgrp_sdsec, geocorr_blkgrp_sduni)
rm(df_blkgrp_sdelm, df_blkgrp_sdsec, df_blkgrp_sduni)

# ASSIGN POLICE DISTRICTS/AGENCIES
# Include 01a_police_agencies.R

# Combine block groups from Nassau, Bergen, and Queens
df_blkgrp_service_units = bind_rows(
  df_blkgrp_service_units,
  df_queens_service_units
)

df_blkgrp_service_units = df_blkgrp_service_units %>%
  left_join(df_blkgrp_police, by = join_by(blkgrp))

# Places may be used as pseudo-block groups.
# Assign service unit IDs to all places:
# Municipalities (places)
# County subdivisions
# School districts: Elementary? Secondary? Both?
# Police districts

# ASSIGN MUNICIPALITIES (PLACES)
df_place_service_units = df_place_class %>%
  rename(place = geoid)

# ASSIGN COUNTY SUBDIVISIONS
fn = "coterminous_borders_ny_nj/data/geocorr2014_place_cousub.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccccddidd"
geocorr_place_cousub = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_place_cousub = geocorr_place_cousub %>% mutate(
  place = paste0(state, placefp14),
  cousub = paste0(county, cousubfp14)
)

tmp = geocorr_place_cousub %>%
  select(place, cousub, share = afact) %>%
  group_by(place) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share) %>%
  inner_join(df_cousub_class, by = join_by(cousub == geoid)) %>%
  select(-cousub)

# Inner join removes places not in Bergen or Nassau County
df_place_service_units = df_place_service_units %>%
  inner_join(tmp, by = join_by(place))

rm(fn, col_names, col_types, tmp)
rm(geocorr_place_cousub)

# ASSIGN SCHOOL DISTRICTS

fn = "coterminous_borders_ny_nj/data/geocorr2014_place_sduni.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_place_sduni = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_place_sduni = geocorr_place_sduni %>% mutate(
  place = paste0(state, placefp14),
  sduni = if_else(is.na(sduni14), NA, paste0(state, sduni14))
)

fn = "coterminous_borders_ny_nj/data/geocorr2014_place_sdelm.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_place_sdelm = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_place_sdelm = geocorr_place_sdelm %>% mutate(
  place = paste0(state, placefp14),
  sdelm = if_else(is.na(sdelm14), NA, paste0(state, sdelm14))
)

fn = "coterminous_borders_ny_nj/data/geocorr2014_place_sdsec.csv"
col_names = str_to_lower(names(read_csv(fn, n_max = 0)))
col_types = "ccccccccddidd"
geocorr_place_sdsec = read_csv(fn, col_names = col_names, col_types = col_types, skip = 2)
geocorr_place_sdsec = geocorr_place_sdsec %>% mutate(
  place = paste0(state, placefp14),
  sdsec = if_else(is.na(sdsec14), NA, paste0(state, sdsec14))
)


df_place_sduni = geocorr_place_sduni %>%
  select(place, sduni, share = afact) %>%
  group_by(place) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)
df_place_sdelm = geocorr_place_sdelm %>%
  select(place, sdelm, share = afact) %>%
  group_by(place) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)
df_place_sdsec = geocorr_place_sdsec %>%
  select(place, sdsec, share = afact) %>%
  group_by(place) %>%
  slice_max(share) %>%
  ungroup() %>%
  select(-share)

tmp = df_place_sduni %>%
  inner_join(df_place_sdelm, by = join_by(place)) %>%
  inner_join(df_place_sdsec, by = join_by(place)) %>%
  mutate(sdelm = coalesce(sduni, sdelm), sdsec = coalesce(sduni, sdsec)) %>%
  select(-sduni)

df_place_service_units = df_place_service_units %>%
  left_join(tmp, by = join_by(place))

rm(fn, col_names, col_types, tmp)
rm(geocorr_place_sdelm, geocorr_place_sdsec, geocorr_place_sduni)
rm(df_place_sdelm, df_place_sdsec, df_place_sduni)

# ASSIGN POLICE DISTRICTS/AGENCIES
# Completed above
df_place_service_units = df_place_service_units %>%
  left_join(df_place_police, by = join_by(place))

df_service_units = bind_rows(
  rename(df_blkgrp_service_units, geoid = blkgrp),
  rename(df_place_service_units, geoid = place)
)


#####################################################
# GROUP QUARTERS
#####################################################
# Group quarters include correctional facilities, nursing facilities, and schools
# Investigate tracts with > 5% GQ population in Google Maps
# Remove block groups with likely group quarters population,
#   i.e., institutions that meet the above criteria

 
gq_exclusion = c(
  "340030236023" # Bergen County Jail (14.5% GQ)
  , "340030321042" # Ramapo College of New Jersey (50.8% GQ)
  , "340030423022" # Bergen New Bridge Medical Center (24.8% GQ)
  , "340030544003" # Fairleigh Dickinson University (9.2% GQ)
  , "360593001004" # US Merchant Marine Academy (11.0% GQ)
  , "360593018002" # North Shore University Hospital (9.4% GQ)
  , "360594072041" # Hofstra University (8.5% GQ)
  , "360594073011" # Hofstra University (Only blkgrp in tract, 49.9% GQ)
  , "360594078021" # Nassau University Medical Center (Only blkgrp in tract, 71.8% GQ)
  , "360595182031" # North Shore LIJ Health Systems (21.7% GQ)
  , "360599801001" # Adelphi University (Only blkgrp in tract, 100% GQ)
  , "360599811001" # LIU Post (Only blkgrp in tract, 99.1% GQ)
  , "360599821001" # SUNY College at Old Westbury (Only blkgrp in tract, 98.2% GQ)
  , "360595219021" # This is shore area from East Massapequa that extends to
                  # Jones Beach. Significant water area screws up connectivity
                  # matrix. Could add back in but would have to clip water area.
  
)

# Need to handle Queens group quarters population


###############################################################################
# CREATE PSEUDO-BLOCK GROUPS
###############################################################################

# Combine block groups with places, mix-and-match

################## NEW YORK PLACES

split_blkgrps = unique(geocorr_blkgrp_place$blkgrp[duplicated(geocorr_blkgrp_place$blkgrp)])


# Avoid including LIU Post and SUNY Westbury in split block groups.
# Avoid including two block groups that partially overlap with South Hackensack Township.
# Exclude following IDs from search:
split_blkgrps = split_blkgrps[
  !(split_blkgrps %in% c("360595177051", "360599811001", "360599821001", "360593039003", "340030361001", "340030600001"))
]

# Filter:
#    Keep IDs of split block groups
#    Remove block groups where < 5% of the population is split (keep between 5% and 95%)
#    Remove Place ID 99999
tmp = geocorr_blkgrp_place %>% 
  filter(blkgrp %in% split_blkgrps & afact >= 0.05 & afact < 0.95)

use_places = unique(tmp$place)
# Consider removing 3605034, Bayville, which is comprised of 4 block groups (although one block group falls 21% outside of Bayville)

# Add following places in preference to block groups with significant water area:
use_places = c(use_places, "3632105", "3642279")

# Now find all block groups not in places in the `use_places` vector
# Exclude block groups with significant group quarters population
# Also remove block group parts with < 5% of block group population (retain 95% part)
tmp = geocorr_blkgrp_place %>%
  filter(
    afact >= 0.05 
    & !(place %in% use_places) 
    & !(blkgrp %in% gq_exclusion)
  )

use_blkgrps = tmp$blkgrp
rm(tmp)

# Check for duplicates
length(use_blkgrps[duplicated(use_blkgrps)]) == 0
# [1] "340030361001" "340030600001" "360595177051"
# Duplicates OK. We want to retain these block groups. Following filter will still work.

# Create study dataset by combining block groups and places
gdf_pseudo_blkgrp = bind_rows(
  mutate(filter(gdf_nassau_blkgrp, geoid %in% use_blkgrps), county = "Nassau"),
  mutate(filter(gdf_ny_place, geoid %in% use_places), county = "Nassau"),
  mutate(filter(gdf_bergen_blkgrp, geoid %in% use_blkgrps), county = "Bergen"),
  mutate(filter(gdf_nj_place, geoid %in% use_places), county = "Bergen"),
  mutate(gdf_queens_blkgrp, county = "Queens") # Use all Queens block groups; eventually filter for GQ exclusions
)

# Quick view of results:
# qtm(gdf_pseudo_blkgrp)

###################################################
# ADJUST GEOMETRIES OF PSEUDO-BLOCK GROUPS IN STUDY
###################################################
# Before creating nb object, adjust geometries of three block groups that include parts of South Hackensack Township:
# 340030361001, 340030362001, 340030600001

geo = gdf_pseudo_blkgrp %>%
  filter(geoid == "340030362001") %>%
  st_intersection(filter(gdf_nj_place, geoid == "3447700")) %>%
  st_geometry() %>%
  st_cast("MULTIPOLYGON")
gdf_pseudo_blkgrp[gdf_pseudo_blkgrp$geoid == "340030362001", "geometry"] = geo

geo = gdf_pseudo_blkgrp %>%
  filter(geoid == "340030600001") %>%
  st_intersection(filter(gdf_nj_place, geoid == "3482570")) %>%
  st_geometry() %>%
  st_cast("MULTIPOLYGON")
gdf_pseudo_blkgrp[gdf_pseudo_blkgrp$geoid == "340030600001", "geometry"] = geo

geo = gdf_pseudo_blkgrp %>%
  filter(geoid == "340030361001") %>%
  st_difference(filter(gdf_nj_place, geoid == "3472480")) %>%
  st_geometry() %>%
  st_cast("MULTIPOLYGON")
gdf_pseudo_blkgrp[gdf_pseudo_blkgrp$geoid == "340030361001", "geometry"] = geo
rm(geo)

# Create population center of each block group from Census data
df_center_of_population_blkgrp = read_csv("coterminous_borders_ny_nj/data/CenPop2010_Mean_BG.csv") %>%
  rename_with(tolower) %>%
  filter((statefp == "34" & countyfp == "003") | (statefp == "36" & countyfp %in% c("059", "081"))) %>%
  mutate(blkgrp = paste0(statefp, countyfp, tractce, blkgrpce))

gdf_center_of_population_blkgrp = df_center_of_population_blkgrp %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(26918) %>%
  select(blkgrp) %>%
  rename(geoid = blkgrp)

# Calculate centers of population for places being used as pseudo-block groups
gdf_center_of_population_place = geocorr_blkgrp_place %>%
  filter(place %in% use_places) %>%
  group_by(place) %>%
  summarise(
    longitude = sum(afact * pop10 * intptlon) / sum(afact * pop10),
    latitude = sum(afact * pop10 * intptlat) / sum(afact * pop10)
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(26918) %>%
  rename(geoid = place)

gdf_center_of_population_pseudo_blkgrp = gdf_pseudo_blkgrp %>%
  select(geoid) %>%
  st_drop_geometry() %>%
  left_join(bind_rows(gdf_center_of_population_blkgrp, gdf_center_of_population_place), by = join_by(geoid)) %>%
  st_as_sf()

###################################################
# CONSTRUCT NETWORK FOR PSEUDO-BLOCK GROUPS IN STUDY
###################################################

row.names(gdf_pseudo_blkgrp) = gdf_pseudo_blkgrp$geoid # Necessary, as row.names parameter of poly2nb is ignored for sf objects
nb = poly2nb(gdf_pseudo_blkgrp, queen = FALSE)
lw = nb2listw(nb, style = "B")

# gdf_cxn = listw2lines(lw_blkgrp, st_centroid(st_geometry(gdf_pseudo_blkgrp)), as_sf = TRUE)
# gdf_cxn = listw2lines(lw, st_point_on_surface(st_geometry(gdf_pseudo_blkgrp)), as_sf = TRUE)
gdf_cxn = listw2lines(lw, st_geometry(gdf_center_of_population_pseudo_blkgrp), as_sf = TRUE)

# Remove duplicates:
gdf_cxn = gdf_cxn %>% filter(i < j)

# Keep neighbors in the same county (i.e., remove Queens-Nassau cross-border connections; generalizable to adding other neighboring counties)
# OLD: gdf_cxn = gdf_cxn %>% filter(str_sub(i_ID, 1, 5) == str_sub(j_ID, 1, 5))
# Add county names
gdf_cxn = gdf_cxn %>%
  inner_join(select(st_drop_geometry(gdf_pseudo_blkgrp), geoid, county), join_by(i_ID == geoid)) %>%
  inner_join(select(st_drop_geometry(gdf_pseudo_blkgrp), geoid, county), join_by(j_ID == geoid), suffix = c("", "_j")) %>%
  filter(county == county_j) %>%
  select(-county_j)

# st_write(gdf_cxn, "../GIS/pseudo_block_group_connections.gpkg", driver = "GPKG", append = FALSE)



######################################
# GET SOCIOECONOMIC DATA
######################################

# All API requests are stored in objects prefixed with `zz_` and immediately
# copied to the object intended to be used. This way code other than the 
# API request can be rerun without access to the internet.

# Socioeconomic vars
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
zz_df_bergen_blkgrp = get_acs(
  geography = "block group",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = FALSE
)

zz_df_nassau_blkgrp = get_acs(
  geography = "block group",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = "NY",
  county = "Nassau",
  geometry = FALSE
)

zz_df_queens_blkgrp = get_acs(
  geography = "block group",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = "NY",
  county = "Queens",
  geometry = FALSE
)

# Get place data
zz_df_ny_place = get_acs(
  geography = "place",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = "NY",
  geometry = FALSE
)

zz_df_nj_place = get_acs(
  geography = "place",
  variables = get_vars,
  year = 2019,
  output = "wide",
  state = "NJ",
  geometry = FALSE
)

df_demographics = bind_rows(zz_df_bergen_blkgrp, zz_df_nassau_blkgrp, zz_df_nj_place, zz_df_ny_place, zz_df_queens_blkgrp) %>%
  select(-ends_with("M")) %>%
  filter(GEOID %in% gdf_pseudo_blkgrp$geoid) %>%
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
    , hh_0k = B19001_002E # Less than $10,000
    , hh_10k = B19001_003E # $10,000 to $14,999
    , hh_15k = B19001_004E # $15,000 to $19,999
    , hh_20k = B19001_005E # $20,000 to $24,999
    , hh_25k = B19001_006E # $25,000 to $29,999
    , hh_30k = B19001_007E # $30,000 to $34,999
    , hh_35k = B19001_008E # $35,000 to $39,999
    , hh_40k = B19001_009E # $40,000 to $44,999
    , hh_45k = B19001_010E # $45,000 to $49,999
    , hh_50k = B19001_011E # $50,000 to $59,999
    , hh_60k = B19001_012E # $60,000 to $74,999
    , hh_75k = B19001_013E # $75,000 to $99,999
    , hh_100k = B19001_014E # $100,000 to $124,999
    , hh_125k = B19001_015E # $125,000 to $149,999
    , hh_150k = B19001_016E # $150,000 to $199,999
    , hh_200k = B19001_017E # $200,000 or more
    , mhi = B19013_001E # Median Household Income
    , agg_hh_inc = B19025_001E # Aggregate Household Income
  ) %>%
  rename_with(tolower)

# Estimate missing (suppressed) MHI based on income bins
inc_lbound = 1000 * c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200)
inc_width = lead(inc_lbound, default = 300000) - inc_lbound

# Get name of income bin with median household for each geoid, and values needed to estimate MHI
df_inc = df_demographics %>%
  filter(is.na(mhi)) %>%
  select(geoid, hh_0k, hh_10k, hh_15k, hh_20k, hh_25k, hh_30k, hh_35k, hh_40k, hh_45k, hh_50k, hh_60k, hh_75k, hh_100k, hh_125k, hh_150k, hh_200k) %>%
  pivot_longer(-geoid) %>%
  mutate(hh_cumsum = cumsum(value), hh_total = sum(value), inc_bin = row_number(), .by = geoid) %>%
  filter(hh_cumsum > hh_total / 2) %>%
  slice_head(n = 1, by = geoid) %>% 
  mutate(
    inc_lbound = inc_lbound[inc_bin],
    inc_width = inc_width[inc_bin]
  )

# Calculate MHI by proportional distance of median household within income range
# Drop all other columns
df_inc = df_inc %>%
  mutate(
    mhi2 = inc_lbound + inc_width * ((hh_total/2 - (hh_cumsum - value)) / value)
  ) %>%
  select(geoid, mhi2)

# Update df_demographics with estimated MHIs
df_demographics = df_demographics %>% 
  left_join(df_inc, by = join_by(geoid)) %>%
  mutate(mhi = coalesce(mhi, mhi2)) %>%
  select(-mhi2)

# Calculate Queens precinct populations for later use in crime rates
df_nypd_precincts = df_queens_blkgrp_police %>% 
  inner_join(select(zz_df_queens_blkgrp, geoid = GEOID, population = B01003_001E), by = join_by(geoid)) %>%
  select(-geoid) %>%
  group_by(precinct) %>%
  summarise(population = sum(population))

# Calculate Nassau police district and precinct populations for later use in crime rates
df_nassau_small_agency = df_nassau_place_police %>% 
  filter(control != "County") %>%
  inner_join(select(zz_df_ny_place, geoid = GEOID, population = B01003_001E), by = join_by(geoid)) %>%
  select(police, population) %>%
  group_by(police) %>%
  summarise(population = sum(population))
df_ncpd_precincts = df_nassau_blkgrp_police %>% 
  filter(control == "County") %>%
  inner_join(select(zz_df_nassau_blkgrp, geoid = GEOID, population = B01003_001E), by = join_by(geoid)) %>%
  select(police, population) %>%
  group_by(police) %>%
  summarise(population = sum(population))

# Bergen crime data includes populations

############################################################################
# LOAD SERVICE QUALITY DATA
############################################################################
fn = "~/Data/Stanford Education Data Archive/seda_geodist_poolsub_cs_4.1.csv"
df_sdelm_quality = read_csv(fn, col_types = cols(sedalea = "c", fips = "c")) %>%
  filter(fips %in% c("34", "36"), subcat == "all") %>%
  select(sedalea, sedaleaname, stateabb, cs_mn_avg_mth_ol, cs_mn_avg_rla_ol) %>%
  rename(sdelm_math = cs_mn_avg_mth_ol, sdelm_rla = cs_mn_avg_rla_ol)

df_service_units = df_service_units %>% left_join(
  select(df_sdelm_quality, sedalea, sdelm_math, sdelm_rla),
  by = join_by(sdelm == sedalea)
)

# QUEENS CRIME
df_nypd_crime = read_csv("coterminous_borders_ny_nj/data/nypd-seven-major-felony-offenses-by-precinct-2000-2022.csv")
df_nypd_crime = df_nypd_crime %>%
  rename_with(tolower) %>%
  select(precinct, crime, `2019`) %>%
  filter(precinct != "DOC", crime != "TOTAL SEVEN MAJOR FELONY OFFENSES") %>%
  pivot_wider(id_cols = precinct, names_from = crime, values_from = `2019`) %>%
  rename_with(tolower) %>%
  rename(
    murder = `murder & non negl. manslaughter`, 
    assault = `felony assault`, 
    larceny = `grand larceny`, 
    motor_vehicle_theft =`grand larceny of motor vehicle`
  ) %>%
  mutate(
    violent_crime = murder + rape + robbery + assault, 
    property_crime = burglary + larceny + motor_vehicle_theft
  )
df_queens_crime = df_nypd_precincts %>%
  inner_join(df_nypd_crime, by = join_by(precinct)) %>%
  mutate(violent_crime_rate = 100000 * violent_crime / population, property_crime_rate = 100000 * property_crime / population) %>%
  select(police = precinct, violent_crime_rate, property_crime_rate) %>%
  mutate(
    police = paste("NYPD", police), 
    z_violent = as.vector(scale(violent_crime_rate)), 
    z_property = as.vector(scale(property_crime_rate))
  ) #mutate(police = paste("NYPD", police))

# NASSAU CRIME

fn = "coterminous_borders_ny_nj/data/nassau_county_ucr_by_agency_2019.csv"
if (file.exists(fn)) {
  df_nassau_crime = read_csv(fn)
} else {
  qry = "https://data.ny.gov/resource/ca8h-8gjq.csv?$query=SELECT%0A%20%20%60county%60%2C%0A%20%20%60agency%60%2C%0A%20%20%60year%60%2C%0A%20%20%60months_reported%60%2C%0A%20%20%60total_index_crimes%60%2C%0A%20%20%60violent%60%2C%0A%20%20%60murder%60%2C%0A%20%20%60forcible_rape%60%2C%0A%20%20%60robbery%60%2C%0A%20%20%60aggravated_assault%60%2C%0A%20%20%60property%60%2C%0A%20%20%60burglary%60%2C%0A%20%20%60larceny%60%2C%0A%20%20%60motor_vehicle_theft%60%2C%0A%20%20%60region%60%0AWHERE%20(%60county%60%20IN%20(%22Nassau%22%2C%20%22Queens%22))%20AND%20(%60year%60%20IN%20(%222019%22))%0AORDER%20BY%0A%20%20%60year%60%20DESC%20NULL%20FIRST%2C%0A%20%20%60county%60%20ASC%20NULL%20LAST%2C%0A%20%20%60agency%60%20ASC%20NULL%20LAST%2C%0A%20%20%60region%60%20ASC%20NULL%20LAST"
  df_nassau_crime = read_csv(qry)
  # Store local version
  write_csv(df_nassau_crime, fn)
}

# Extract NCPD for later use
ncpd_ucr_2019 = filter(df_nassau_crime, agency == "Nassau County PD")

# Now handle all small police agencies
df_nassau_small_agency_crime = df_nassau_crime %>%
  filter(agency != "County Total") %>% # rename(police = agency) %>%
  select(police = agency, violent_crime = violent, property_crime = property) %>%
  inner_join(df_nassau_small_agency, by = join_by(police)) %>%
  mutate(
    violent_crime_rate = 100000 * violent_crime / population, 
    property_crime_rate = 100000 * property_crime / population
  ) %>%
  select(police, violent_crime_rate, property_crime_rate)

# NCPD crime by precinct taken from PDF reports, only shows first 8 or 10 months of 2019.
# Construct data frame of first 8 months, use that to allocate full-year NCPD crime
# taken from UCR.
police = paste("NCPD", 1:8)
murder = c(2, 1, 0, 0, 0, 0, 3, 2)
rape = c(3, 0, 2, 0, 1, 0, 0, 0)
robbery = c(33+8, 6+4, 29+30, 9+9, 25+13, 4+3, 6+2, 14+8)
assault = c(46, 13, 64, 35, 39, 8, 11, 22)
burglary = c(36+39, 11+24, 41+43, 15+13, 28+16, 9+6, 18+16, 26+33)
motor_vehicle_theft = c(62, 17, 29, 54, 76, 17, 39, 29)
larceny = c(227, 237, 677, 225, 362, 220, 233, 197)

df_ncpd_precinct_crime = data.frame(
  police, 
  murder, rape, robbery, assault,
  burglary, motor_vehicle_theft, larceny
  ) %>%
  mutate(
    violent_crime = murder + rape + robbery + assault, 
    property_crime = burglary + larceny + motor_vehicle_theft
  ) %>%
  inner_join(df_ncpd_precincts, by = join_by(police)) 

ncpd_violent_ratio = ncpd_ucr_2019$violent / sum(df_ncpd_precinct_crime$violent_crime)
ncpd_property_ratio = ncpd_ucr_2019$property / sum(df_ncpd_precinct_crime$property_crime)

# Multiply all precincts by ratio before calculating rate per 100,000 population
df_ncpd_precinct_crime = df_ncpd_precinct_crime %>%
  mutate(
    violent_crime = ncpd_violent_ratio * violent_crime, 
    property_crime = ncpd_property_ratio * property_crime,
    violent_crime_rate = 100000 * violent_crime / population, 
    property_crime_rate = 100000 * property_crime / population
  ) %>%
  select(police, violent_crime_rate, property_crime_rate)

df_nassau_crime = bind_rows(
  df_nassau_small_agency_crime,
  df_ncpd_precinct_crime
)
df_nassau_crime = df_nassau_crime %>%
  mutate(
    z_violent = as.vector(scale(violent_crime_rate)), 
    z_property = as.vector(scale(property_crime_rate))
  )

# BERGEN CRIME
df_bergen_crime = read_csv("coterminous_borders_ny_nj/data/bergen_ucr_2019.csv") %>%
  rename_with(tolower) %>%
  filter(attribute == "Number of Offenses") %>%
  select(-orinumber, -months, -total, -attribute) %>%
  rename(police = agency, motor_vehicle_theft = `auto theft`) %>%
  mutate(
    violent_crime = murder + rape + robbery + assault, 
    property_crime = burglary + larceny + motor_vehicle_theft,
    violent_crime_rate = 100000 * violent_crime / population, 
    property_crime_rate = 100000 * property_crime / population
  ) %>%
  select(police, violent_crime_rate, property_crime_rate) %>%
  filter(!(is.nan(violent_crime_rate) | is.infinite(violent_crime_rate))) %>%
  mutate(
    z_violent = as.vector(scale(violent_crime_rate)), 
    z_property = as.vector(scale(property_crime_rate))
  )
  
# df_public_safety_backup = df_public_safety
df_public_safety = bind_rows(
  df_queens_crime, df_nassau_crime, df_bergen_crime
)

df_service_units = df_service_units %>% 
  left_join(df_public_safety, by = join_by(police))

############################################################################
# CALCULATE DIFFS BETWEEN NEIGHBORING UNITS
############################################################################

df_demographics = df_demographics %>% select(-name)

df_analysis = gdf_cxn %>%
  mutate(
    bergen = county == "Bergen", nassau = county == "Nassau", 
    queens = county == "Queens", suburban = !queens,
    distance_km = as.numeric(st_length(geometry))/1000, distance_km2 = distance_km^2
  ) %>%
  st_drop_geometry() %>%
  select(-i, -j, -wt) %>%
  rename(i_geoid = i_ID, j_geoid = j_ID)

# Add service unit IDs
df_analysis = df_analysis %>%
  left_join(df_service_units, by = join_by(i_geoid == geoid)) %>%
  left_join(df_service_units, by = join_by(j_geoid == geoid), suffix = c("_i", "_j"))

# Classify by service area borders or differences
df_analysis = df_analysis %>% mutate(
  muni = coalesce(c1_i, c5_i, "not in muni") != coalesce(c1_j, c5_j, "not in muni"),
  township = coalesce(t1_i, "not in township") != coalesce(t1_j, "not in township"),
  sdelm = sdelm_i != sdelm_j, # All pseudo-BGs should be assigned to a school district
  sdelm_math_diff = abs(sdelm_math_i - sdelm_math_j),
  sdelm_rla_diff = abs(sdelm_rla_i - sdelm_rla_j),
  sdelm_comb_diff = (sdelm_math_diff + sdelm_rla_diff) / 2,
  sdsec = sdsec_i != sdsec_j, # All pseudo-BGs should be assigned to a school district
  police = police_i != police_j, # All pseudo-BGs should be assigned to a  police agency
  violent_diff = abs(violent_crime_rate_i - violent_crime_rate_j),
  property_diff = abs(property_crime_rate_i - property_crime_rate_j),
  z_violent_diff = abs(z_violent_i - z_violent_j),
  z_property_diff = abs(z_property_i - z_property_j)
)

# Treat NJ townships as municipalities
df_analysis = df_analysis %>% mutate(
  muni = muni | (bergen & coalesce(t1_i, "not in township") != coalesce(t1_j, "not in township"))
)

# Add demographic data
df_analysis = df_analysis %>%
  inner_join(df_demographics, by = join_by(i_geoid == geoid)) %>%
  inner_join(df_demographics, by = join_by(j_geoid == geoid), suffix = c("_i", "_j"))

# Difference in Median Household Income
df_analysis = df_analysis %>% mutate(
  mhi_diff = abs(mhi_i - mhi_j),
  mhi_ln_diff = abs(log(mhi_i) - log(mhi_j))
)

# Race/ethinicty diffs
df_analysis = df_analysis %>% mutate(
  black_diff = abs((black_i/population_i) - (black_j/population_j)),
  hispanic_diff = abs((hispanic_i/population_i) - (hispanic_j/population_j)),
  white_diff = abs((white_i/population_i) - (white_j/population_j)),
  asian_diff = abs((asian_i/population_i) - (asian_j/population_j)),
  children_diff = abs((hh_children_i/hh_all_i) - (hh_children_j/hh_all_j))
)
  

