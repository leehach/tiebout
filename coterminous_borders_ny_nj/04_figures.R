library(tidyverse)
library(ggeffects)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmap)
library(vcd)

ggplot(df, aes(x = sdelm_rla_diff, y = mhi_diff, color = as.logical(muni))) +
  geom_smooth(method = "lm")

ggplot(df, aes(x = sdelm_math_diff, y = mhi_diff, color = as.logical(muni))) +
  geom_smooth(method = "lm")

ggplot(df, aes(x = violent_diff, y = mhi_diff, color = as.logical(muni))) +
  geom_smooth(method = "lm")

ggplot(df, aes(x = property_diff, y = mhi_diff, color = as.logical(muni))) +
  geom_smooth(method = "lm")

df_mosaic = df %>%
  mutate(county = as.character(county)) %>%
  filter(county != "Queens")
mosaic(~ muni + county + sdelm + police, data = df_mosaic)

600000 - 630000
4530000 - 4511000

gdf_ny_school_districts = school_districts(
  state = "NY", 
  type = "unified",
  cb = TRUE,
  year = 2019
)

tm_shape(gdf_nassau, bbox = c(605000, 4513000, 630000, 4529000)) +
  tm_polygons(col = "white") +
tm_shape(filter(gdf_ny_place, classfp %in% c("C1", "C5"))) +
  tm_polygons() +
tm_shape(gdf_ny_school_districts) +
  tm_borders(col = "red", lwd = 3) +
tm_shape(gdf_nassau_police_districts) +
  tm_borders(col = "blue", lwd = 2, lty = "dotted") +
tm_shape(filter(gdf_ny_place, classfp %in% c("C1", "C5"))) +
  tm_text("name", size = "AREA", bg.color = "gray85", overwrite.lines = TRUE) +
tm_layout(bg.color = "lightcyan1") +
tm_add_legend(
  title = "Service Borders",
  type = "fill",
  col = c("gray", "white"),
  labels = c("Municipality", "No Municipal Government")
) +
tm_add_legend(
  type = "line", 
  col = c("red", "blue"),
  lwd = c(3, 2),
  lty = c("solid", "dotted"),
  labels = c("School District", "Policy Agency/Precinct")
)

# Marginal Effect Plots

#Model E
plot(ggpredict(lm1e, c("sdelm", "police", "muni")), connect.lines = TRUE) +
  labs(
    x = "Elementary School District Border",
    y = "Median Household Income, Absolute Difference",
    col = "Police Agency Border",
    title = NULL,
    subtitle = "Municipal Border"
  )

# Model F
plot(ggpredict(lm1f, c("sdelm_rla_diff", "muni"))) +
  labs(
    x = "Elementary School District\nDifference in Reading & Language Arts (z-scores)",
    y = "Median Household Income, Absolute Difference",
    col = "Municipal\nBorder",
    title = NULL
  )
plot(ggpredict(lm1f, c("property_diff", "muni"))) +
  labs(
    x = "Police Agency, Difference in Property Crimes per 100,000",
    y = "Median Household Income, Absolute Difference",
    col = "Municipal\nBorder",
    title = NULL
  )

# Model G
plot(ggpredict(lm1g, c("sdelm_rla_diff", "children_diff", "muni"))) +
  labs(
    x = "Elementary School District\nDifference in Reading & Language Arts (z-scores)",
    y = "Median Household Income, Absolute Difference",
    col = "Proportion of\nHouseholds\nwith Children,\nAbs. Difference",
    title = NULL,
    subtitle = "Municipal Border"
  )

# Model H
plot(ggpredict(lm1h, c("white_diff", "muni"))) +
  labs(
    x = "Proportion in White Population, Absolute Difference",
    y = "Median Household Income, Absolute Difference",
    col = "Municipal Border",
    title = NULL
  )

