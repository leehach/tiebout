library(tidyverse)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

###################################
# The problem was that Compind pkg depends from spdep. pkg. But the new R 4.1.1 (2021-08-10) and has some issue with some of the library such proj and some external libraries have to be updated. After updating and re-installing spdep from github repos it all worked. – 
# Silchara
# Sep 2, 2021 at 23:52
##############################################
# Census tracts in Bergen
df_bergen = get_acs(
  geography = "tract",
  variables = "B19013_001",
  year = 2019,
  output = "wide",
  state = "NJ",
  county = "Bergen",
  geometry = TRUE
)

df_bergen = st_transform(df_bergen, 26918)
