
# -------------- Load Libraries --------------

# Packages from Lecture Practical Sheets
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
# tmaptools::palette_explorer()

source("constants.R")

# -------------- Load OneMap Planning Area Dataset  --------------

# Load Dataset
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL

# Convert to EPSG:3414
sf_subzone <- st_transform(sf_subzone, crs=3414)

# Create Planning Area and Region level Polygons
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))
sf_region <- sf_subzone %>% group_by(region) %>% summarise(geometry = st_union(geometry))



# -------------- Load Skyrise Greenery Dataset --------------
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)

# Number of Skyrise Greenery Records
nrow(sf_skyrise_greenery)


# -------------- EDA: Types of Skyrise Greenery and their locations --------------

# Reassigning MSCP, HDB and Private Residentials to Residentials
sf_skyrise_greenery$type <- unlist(apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  if (x["type"] %in% c("HDB", "MSCP", "Private Residential")) {
    return("Residential")
  } else {
    return(x["type"])
  }
}))


# Inspect Number of Skyrise Greenery of each Types
dplyr::count(sf_skyrise_greenery, type, sort=TRUE)

# Inspect Visually
ggplot(sf_skyrise_greenery, aes(x=type)) + 
  geom_bar(fill = "darkgreen", alpha = 0.7) +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("Types of Skyrise Greenery Buildings") +
  labs(x = "Type", y = "Count")

tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_skyrise_greenery, title="Type") + tm_symbols(col="type") +
  tm_style(global.style) +
  tm_layout(
    title = "Skyrise Greenery in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)



# -------------- EDA: Most Common Type of Skyrise Building across Planning Areas in Singapore --------------

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




top_skyrise_top_for_each_planning_area <- sf_planning_area_w_skyrise_greenery_info %>% 
                                                                  arrange(desc(n)) %>%
                                                              group_by(pln_area.x) %>% 
                                                                top_n(n=1, wt = n)

tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(top_skyrise_top_for_each_planning_area) + tm_polygons(title="Type", col="type") + tm_borders(col = "grey", alpha = 0.4) +
  tm_style(global.style) +
  tm_layout(
    title = "Most Common Type of Skyrise Greenery in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# -------------- EDA: Skyrise Building across Planning Areas in Singapore --------------

# Overall
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_planning_area_w_skyrise_greenery_info %>% group_by(pln_area.x) %>% summarise(n=sum(n))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,20)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Skyrise Greenery in each Planning Area",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)



# HDB and MSCP (Residential)
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("HDB"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,20)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "HDB and MSCP with Skyrise Greenery in each Planning Area",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)



# Hotels
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


# Commercial Buildings
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Commercial"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,15,20,25,30)) + tm_borders(alpha=0.9) +    
  tm_style(global.style) +
  tm_layout(
    title = "Green Commercial Buildings in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Retail Buildings
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail")))$n)
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,3,6,9,12,15)) + tm_borders(alpha=0.9) +
  tm_style(global.style) +
  tm_layout(
    title = "Green Retail Malls in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


