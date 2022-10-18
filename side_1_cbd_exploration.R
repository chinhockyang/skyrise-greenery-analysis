
# -------------- Load Libraries and Variables --------------

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
# tmaptools::palette_explorer()

source("constants.R")
source("1_initial_exploration.R")

# -------------- 1. Load Urban Design Area region  --------------

urban_area <- readOGR("data/urban_design_area")
sf_urban_area <- st_make_valid(st_as_sf(urban_area))
sf_urban_area <- st_transform(sf_urban_area, crs=3414)

# Quick EDA
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_urban_area) + tm_fill(col="blue", alpha = 0.9)


# -------------- 2. "Clipping" of Urban Areas in Central Region  --------------

# Urban Area in Central Region
sf_urban_area_in_central <- st_intersection(sf_urban_area, sf_region %>% filter(region == "CENTRAL REGION"))

# 5 KM - Buffer from Downtown Core Centroid
sf_downtown_core_buffer <- st_buffer(sf_planning_area %>% filter(pln_area == "DOWNTOWN CORE") %>% st_centroid(), 5000)

tmap_mode("plot")
tm_shape(sf_urban_area_in_central) + tm_fill(col="grey") + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_downtown_core_buffer) + tm_fill(col="yellow", alpha = 0.1) + tm_borders(col="orange")

# Urban Area in CBD
 sf_urban_area_in_cbd <- st_intersection(sf_urban_area_in_central, sf_downtown_core_buffer)


# -------------- 3. "Clipping" of Hotel/Retail/Commercial Skyrise Greenery in Urban CBD  --------------

sf_skyrise_greenery_within_cbd <- st_intersection(sf_skyrise_greenery, sf_urban_area_in_cbd %>% st_union())

# -------------- 4. Number of Skyrise Greenery of each types --------------

sf_skyrise_greenery_within_cbd %>% group_by(type) %>% count()
ggplot(sf_skyrise_greenery_within_cbd, aes(x=type)) + geom_bar(fill = "darkgreen", alpha = 0.7)


# -------------- 5. Geospatial EDA --------------

tmap_mode("plot")
tm_shape(sf_urban_area_in_cbd) + tm_fill(col="grey", alpha=0.2) + tm_borders() +
  tm_style(global.style) +
  tm_layout(
    title = "Skyrise Greenery in CBD",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_shape(sf_skyrise_greenery_within_cbd %>% filter(type %in% c("Hotel", "Retail", "Commercial"))) + 
  tm_dots(size=0.1, col="type", alpha = 1, palette="Set1", id="name") +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)
