# SPECIAL HANDLING FOR POLICE AGENCIES/DISTRICTS
# 
# In Bergen county, police agencies map 1-to-1 onto municipalities.
# Index crimes are reported separately, populations allow rate calculation
# 
# In Nassau county, some villages have their own agencies, or occasionally
# share an agency with a neighboring village. Some villages and all
# unincorporated areas are served by Nassau County PD. NCPD data was
# requested by FOIL, but not made available. Use NCPD UCR totals but 
# allocate to precincts by incomplete 2019 Strat-Com reports (PDF).
# 
# In Queens county, city divided into precincts. Precinct crime data
# available. Need to calculate populations from aggregated block groups,
# calculate rates from populations.

#####################################################
# ASSIGN SERVICE UNIT IDs
#####################################################

# QUEENS COUNTY
# Assign all block groups to precincts by spatial join
fn = "../Data/Police Precincts.geojson"
if (file.exists(fn)) {
  gdf_nypd_precincts = read_sf(fn)
} else {
  gdf_nypd_precincts = read_sf("https://data.cityofnewyork.us/api/geospatial/78dh-3ptz?method=export&format=GeoJSON")
}
gdf_nypd_precincts = gdf_nypd_precincts %>% st_transform(26918) %>% select(precinct)
gdf_queens_blkgrp_police = gdf_queens_blkgrp %>% 
  st_join(gdf_nypd_precincts, largest = TRUE) %>%
  mutate(precinct = paste("NYPD", precinct)) %>%
  rename(police = precinct)
df_queens_blkgrp_police = select(st_drop_geometry(gdf_queens_blkgrp_police), geoid, police)

# BERGEN COUNTY
# Places used for very small number of pseudo-blockgroups. Cousub has to have combination
# of place (c5) and cousub (t1) geoids, then used to join to matching blockgroup's
# matching LSAD.
df_bergen_crosswalk = read_csv("coterminous_borders_ny_nj/data/bergen_pd_muni_crosswalk.csv")
df_bergen_place_police = select(st_drop_geometry(gdf_nj_place), c5 = geoid, name) %>%
  inner_join(df_bergen_crosswalk, by = join_by(name)) %>%
  select(-namelsad) %>%
  rename(police = agency)
df_bergen_cousub_police = select(st_drop_geometry(gdf_nj_cousub), name, t1 = geoid) %>%
  left_join(select(df_bergen_place_police, c5, name), by = join_by(name)) %>%
  inner_join(df_bergen_crosswalk, by = join_by(name)) %>%
  select(-namelsad) %>%
  rename(police = agency)

df_bergen_blkgrp_police = inner_join(
  mutate(df_blkgrp_service_units, blkgrp, muni = coalesce(c5, t1), .keep = "none"),
  mutate(df_bergen_cousub_police, muni = coalesce(c5, t1), police, .keep = "none"),
  by = join_by(muni)
) %>%
  select(-muni)

# NASSAU COUNTY

df_nassau_crosswalk = read_csv("coterminous_borders_ny_nj/data/nassau_police_crosswalk.csv")

# Assign all block groups to precincts by spatial join
fn = "../Data/specialdist_police.shp"
if (file.exists(fn)) {
  gdf_nassau_police_districts = read_sf(fn)
} else {
  warning("Nassau County Police Districts file not available on the local filesystem at the expected location.")
}
gdf_nassau_police_districts = gdf_nassau_police_districts %>% 
  rename_with(tolower) %>%
  filter(county == "Nassau") %>%
  st_transform(26918) %>%
  select(pdname, control, districtid) %>%
  left_join(select(df_nassau_crosswalk, pdname, agency), by = join_by(pdname)) %>%
  mutate(police = case_when(control == "County" ~ paste("NCPD", pdname), .default = coalesce(agency, pdname))) %>%
  select(police, control)

gdf_nassau_place_police = gdf_ny_place %>% st_join(gdf_nassau_police_districts, largest = TRUE)
df_nassau_place_police = select(st_drop_geometry(gdf_nassau_place_police), geoid, name, police, control)
gdf_nassau_blkgrp_police = gdf_nassau_blkgrp %>% st_join(gdf_nassau_police_districts, largest = TRUE)
df_nassau_blkgrp_police = select(st_drop_geometry(gdf_nassau_blkgrp_police), geoid, police, control)


# MERGE ALL DATASETS
df_place_police = bind_rows(
  select(df_bergen_place_police, place = c5, police),
  select(df_nassau_place_police, place = geoid, police)
)

df_blkgrp_police = bind_rows(
  df_bergen_blkgrp_police,
  select(df_nassau_blkgrp_police, blkgrp = geoid, police),
  select(df_queens_blkgrp_police, blkgrp = geoid, police)
)
