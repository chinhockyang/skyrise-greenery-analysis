
# -------------- Load Libraries --------------
library(rgdal)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

# -------------- OneMap Planning Area Dataset  --------------
sg_planning_area <- readOGR("data/onemap_planning_area")

# Issue: Some geometry are invalid (check using st_is_valid) after applying st_as_sf
# Overcome by using st_make_valid function
sf_sg_planning_area <- st_make_valid(st_as_sf(sg_planning_area))

tmap_mode("plot")
tm_shape(sf_sg_planning_area) +
  tm_fill(col="yellow", alpha=0.1) +
  tm_style("classic") +
  tm_borders(alpha=0.8) +
  tm_text(text = "pln_area_n", size = 0.4, col="brown") +
  tm_layout(
    title = "Singapore Planning Area", 
    title.size = 2, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.02,0.03,0.1,0.03)
  ) +
tm_scale_bar(position = c("right", "bottom")) +
tm_compass(type="rose", position=c("right", "top"), show.labels = 2, size = 2)


# -------------- Cycling Path  --------------
# Issue: PLANNING_A and PLANNING_1 <NA>
sp_cycling_path <- readOGR("data/lta_cycling_path")

# -------------- Footpath  --------------
sp_footpath <- readOGR("data/lta_footpath")
sf_footpath <- st_as_sf(sp_footpath)

# Visualization
tmap_mode("plot")
tm_shape(sf_sg_planning_area) + tm_fill(col="yellow", alpha=0.3) +
  tm_borders(alpha=0.5) +
  tm_style("natural") +
  tm_layout(
      title = "Footpaths in Singapore", 
      title.size = 2,
      title.fontface = "bold",
      title.fontfamily = "Palatino",
      title.position=c("left", "top"), 
      inner.margins = c(-0.02,0.03,0.1,0.03)
  ) +
tm_shape(sf_footpath) + tm_lines(lwd=0.6, alpha=0.6, col="darkgreen") +
tm_scale_bar(position = c("right", "bottom")) +
tm_compass(type="rose", position=c("right", "top"), show.labels = 3, size = 2)

