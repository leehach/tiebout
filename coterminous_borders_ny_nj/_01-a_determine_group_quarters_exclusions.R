# Determine excluded block groups with significant group quarters population
gdf_bergen_tracts = get_acs(
  geography = "tract",
  variables = c("B01003_001", "B26001_001"),
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = TRUE
) %>% st_transform(26918)

gdf_nassau_tracts = get_acs(
  geography = "tract",
  variables = c("B01003_001", "B26001_001"),
  year = 2019,
  output = "wide",
  state = "NY",
  county = "Nassau",
  geometry = TRUE
) %>% st_transform(26918)

gdf_gq_tracts = bind_rows(gdf_bergen_tracts, gdf_nassau_tracts)
rm(gdf_bergen_tracts, gdf_nassau_tracts)

gdf_gq_tracts = gdf_gq_tracts %>%
  select(-ends_with("M")) %>%
  rename(population = B01003_001E, gq = B26001_001E) %>%
  rename_with(tolower) %>%
  mutate(gq_pct = 100 * gq / population)

write_sf(gdf_gq_tracts, "../Data/group_quarters_tracts.gpkg", append = FALSE)

###########################################
gdf_gq_bergen_blkgrp = get_acs(
  geography = "block group",
  variables = c("B01003_001"),
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = TRUE
) %>% st_transform(26918)

gdf_gq_nassau_blkgrp = get_acs(
  geography = "block group",
  variables = c("B01003_001"),
  year = 2019,
  output = "wide",
  state = "NY",
  county = "Nassau",
  geometry = TRUE
) %>% st_transform(26918)

gdf_gq_blkgrp = bind_rows(gdf_gq_bergen_blkgrp, gdf_gq_nassau_blkgrp)
rm(gdf_gq_bergen_blkgrp, gdf_gq_nassau_blkgrp)

gdf_gq_blkgrp = gdf_gq_blkgrp %>%
  select(-ends_with("M")) %>%
  rename(population = B01003_001E) %>%
  rename_with(tolower)

write_sf(gdf_gq_blkgrp, "../Data/group_quarters_blkgrp.gpkg", append = FALSE)
