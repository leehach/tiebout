# No objects created in this script should be reused in a later script

library(tmap)

tm_shape(df_bergen) +
  tm_polygons("B19013_001E")
