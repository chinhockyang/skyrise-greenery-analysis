
# -------------- Load Libraries --------------

library(rgdal)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

# -------------- Inspect OneMap Planning Area Dataset  --------------

# SP
sg_planning_area <- readOGR("data/onemap_planning_area")

# For Some reason there are some geometry that are invalid (check using st_is_valid) after applying st_as_sf
sf_sg_planning_area <- st_make_valid(st_as_sf(sg_planning_area))

tmap_mode("plot")
tm_shape(sf_sg_planning_area) +
  tm_fill(col="yellow", alpha=0.1) +
  tm_borders(alpha=0.8) +
  tm_text(text = "pln_area_n", size = 0.4, col="brown") +
  tm_layout(title = "Singapore Planning Area", title.size = 1.5, title.position=c("center", "top"), inner.margins = 0.08)


# -------------- Inspect LTA Datasets  --------------
# Cycling Path 
######[TODO]: Fix PLANNING_A and PLANNING_1 <NA>
sp_cycling_path <- readOGR("data/lta_cycling_path")

# Footpath
###### Has OBJECTID but no information of the road
sp_footpath <- readOGR("data/lta_footpath")
sf_footpath <- st_as_sf(sp_footpath)
tmap_mode("plot")
tm_shape(sf_sg_planning_area) +
  tm_borders(alpha=0.5) +
  tm_layout(title = "Footpaths in Singapore", title.size = 1.5, title.position=c("center", "top"), inner.margins = 0.08) +
  tm_shape(sf_footpath) + tm_lines(lwd=0.6, alpha=0.6)

