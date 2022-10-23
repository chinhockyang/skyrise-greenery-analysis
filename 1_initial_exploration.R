
# -------------- Load Libraries --------------

# Packages from Lecture Practical Sheets
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
tmaptools::palette_explorer()

source("constants.R")

# -------------- 1. Load OneMap Planning Area Dataset  --------------

# Load Dataset
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL

# Convert to EPSG:3414
sf_subzone <- st_transform(sf_subzone, crs=3414)

# Create Planning Area and Region level Polygons
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))
sf_region <- sf_subzone %>% group_by(region) %>% summarise(geometry = st_union(geometry))



# -------------- 2. Load Skyrise Greenery Dataset --------------
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)

# Quick EDA of All the Skyrise Greenery Locations
tmap_mode("plot")
tm_shape(sf_subzone) + tm_fill(col="grey", alpha=0.2) +
  tm_borders(alpha=0.5) +
  tm_style(global.style) +
  tm_layout(
    title = "Skyrise Buildings in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_shape(sf_skyrise_greenery) + tm_dots(size=0.1, col="type", alpha = 1, palette="Set3") +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# -------------- 3. Containment of Skyrise Greenery within Planning Areas --------------
# Group the Skyrise Greenery by their types and planning areas
sf_skyrise_greenery_grpby_type_and_planning_area <- st_join(sf_skyrise_greenery, 
                                               sf_planning_area, 
                                               join = st_within, 
                                               left = TRUE) %>% 
                                      dplyr::select("name","type","pln_area")%>%
                                      group_by(pln_area, type) %>% 
                                      count()

# Use Join operations to identify number of Skyrise Greenery of each type for each planning areas
sf_planning_area_w_skyrise_greenery_info <- st_join(sf_planning_area, 
                                                sf_skyrise_greenery_grpby_type_and_planning_area, 
                                               join = st_contains)

# -------------- 4. Number of Skyrise Greenery of each types --------------

ggplot(sf_skyrise_greenery, aes(x=type)) + geom_bar(fill = "darkgreen", alpha = 0.7)

# -------------- 5. Geospatial EDA --------------

tmap_mode("plot")

# HDB Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("HDB")))$n)

tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("HDB"))) + 
  tm_polygons("n", alpha=1, title="Skyrise HDB", breaks=c(0,5,10,20)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green HDBs across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# MSCP Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("MSCP")))$n)

tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("MSCP"))) + 
  tm_polygons("n", alpha=1, title="Skyrise HDB", breaks=c(0,2,4,6)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green MSCP across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Schools with Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Education")))$n)

# ==> KIV: To potentially exclude local Unis/Poly/Private Unis from School
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Education"))) + 
  tm_polygons("n", alpha=1, title="Skyrise Greenery", breaks=c(0,5,10,15)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green Schools across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Hotels with Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Hotel")))$n)

tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Hotel"))) + 
  tm_polygons("n", alpha=1, title="Skyrise Greenery", breaks=c(0,5,10,15)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green Hotels across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Commercial Buildings with Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Commercial")))$n)

# ==> KIV: To use Dot Maps instead for clearer representation and more diversed graphs
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Commercial"))) + 
  tm_polygons("n", alpha=1, title="Skyrise Greenery", breaks=c(0,5,10,15,20,25,30)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green Hotels across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Retail Buildings with Skyrise Greenery across Planning Areas
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail")))$n)

# ==> KIV: To use Dot Maps instead for clearer representation and more diversed graphs
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail"))) + 
  tm_polygons("n", alpha=1, title="Skyrise Greenery", breaks=c(0,5,10,15)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green Hotels across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)
