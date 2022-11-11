# Initial Exploration of Skyrise Greenery

# Load Libraries and Variables
#===============================================================

# Data Files Required
# 1. onemap_subzone (shp folder)
# 2. skyrise_greenery (shp folder)

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
# tmaptools::palette_explorer()
source("constants.R")

# Preparing OneMap Planning Area dataset (Preprocessing)
#===============================================================
# Load Dataset
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL

# Convert to EPSG:3414
sf_subzone <- st_transform(sf_subzone, crs=3414)

# Create Planning Area and Region level Polygons
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))
sf_region <- sf_subzone %>% group_by(region) %>% summarise(geometry = st_union(geometry))


# Preparing Skyrise Greenery dataset (Preprocessing)
#===============================================================
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)

# Number of Skyrise Greenery Records
nrow(sf_skyrise_greenery)

# Reassigning MSCP, HDB and Private Residentials to Residentials
sf_skyrise_greenery$type <- unlist(apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  if (x["type"] %in% c("HDB", "MSCP", "Private Residential")) {
    return("Residential")
  } else {
    return(x["type"])
  }
}))

# Skyrise Greenery Exploration
#===============================================================

# Dot Map - Skyrise Greenery
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_skyrise_greenery, title="Type") + tm_symbols(size = 0.2) +
  tm_layout(
    title = "Skyrise Greenery in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Skyrise Greenery Types
#===============================================================

# Inspect Number of Skyrise Greenery of each Types
dplyr::count(sf_skyrise_greenery, type, sort=TRUE)

# Inspect Visually through Bar Plot
ggplot(sf_skyrise_greenery, aes(y=type)) + 
  geom_bar(fill = "darkorange", alpha = 0.7) +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("Types of Skyrise Greenery Buildings") +
  labs(x = "Type", y = "Count") +
  scale_fill_brewer(palette = "Pastel1")  

# Inspect Visually through Pie Chart
ggplot(dplyr::count(sf_skyrise_greenery, type, sort=TRUE), aes(x="", y = n, fill = type)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")


# Dot Map - Skyrise Greenery and their Types
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_skyrise_greenery, title="Type") + tm_symbols(col="type", size=0.05, alpha=0.8) +
  tm_style(global.style) +
  tm_layout(
    title = "Skyrise Greenery in Singapore (and their types)", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

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

# Most common skyrise greenery type for each planning area
top_skyrise_top_for_each_planning_area <- sf_planning_area_w_skyrise_greenery_info %>% 
                                                                  arrange(desc(n)) %>%
                                                              group_by(pln_area.x) %>% 
                                                                top_n(n=1, wt = n)

# Visualisation - Most common skyrise greenery type for each planning area
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


# Skyrise Greenery across Planning Area
#===============================================================

# Visualisation - Number of Skyrise Greenery in each Planning Area
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_planning_area_w_skyrise_greenery_info %>% group_by(pln_area.x) %>% summarise(n=sum(n))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,15,20)) + tm_borders(alpha=0.9) +    
  tm_layout(
    title = "Skyrise Greenery in each Planning Area",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Visualisation - Number of Residential Skyrise Greenery in each Planning Area
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Residential"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,20)) + tm_borders(alpha=0.9) +    
  tm_layout(
    title = "Residential Skyrise Greenery in each Planning Area",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Visualisation - Number of Hotels Skyrise Greenery in each Planning Area
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Hotel"))) + 
  tm_polygons("n", alpha=1, title="Skyrise Greenery", breaks=c(0,5,10,15)) + tm_borders(alpha=0.9) +    
  tm_layout(
    title = "Hotel Skyrise Greenery in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Visualisation - Number of Commercial Skyrise Greenery in each Planning Area
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Commercial"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,5,10,15,20,25,30)) + tm_borders(alpha=0.9) +    
  tm_layout(
    title = "Commercial Skyrise Greenery in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Visualisation - Number of Retail Skyrise Greenery in each Planning Area
quantile((sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail")))$n)
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(sf_planning_area_w_skyrise_greenery_info %>% filter(type %in% c("Retail"))) + 
  tm_polygons("n", alpha=1, title="Number of Skyrise Greenery", breaks=c(0,3,6,9,12,15)) + tm_borders(alpha=0.9) +
  tm_layout(
    title = "Retail Skyrise Greenery in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Point Pattern Analysis
#===============================================================

# -------------- Density Related Skyrise Greenery Analysis --------------

require(spatstat)
library(raster)
library(oldtmaptools)

# KDE
skyrise_dens <- smooth_map(sf_skyrise_greenery, bandwidth = choose_bw(sf_skyrise_greenery$geometry))
tmap_mode('view')
tm_shape(skyrise_dens$raster) + tm_raster(title="Count") +
  tm_shape(sf_planning_area) + tm_fill() + tm_borders(alpha=0.5) +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# Hexagon Bin
skyrise_hex <- hexbin_map(as(sf_skyrise_greenery, "Spatial"), bins=25)
tm_shape(skyrise_hex) + tm_fill(col='z',title='Count',alpha=1) +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# Quadrat
Q <- quadratcount(as.ppp(sf_skyrise_greenery), nx=8, ny=6)
plot(as.ppp(sf_skyrise_greenery), pch=20, cols="grey80", main=NULL)  # Plot points
plot(Q, add=TRUE)

Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE, col = heat.colors(2)), main=NULL, las=1)  # Plot density raster
plot(as.ppp(sf_skyrise_greenery), pch=10, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Kernel Density
K1 <- density(as.ppp(sf_skyrise_greenery))
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)


# -------------- Distance Related Skyrise Greenery Analysis --------------
## k-function
kf <- Kest(as.ppp(sf_skyrise_greenery$geometry), correction = 'border')
plot(kf)
plot(kf, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf.env <- envelope(as.ppp(sf_skyrise_greenery$geometry),Kest,correction="border")
plot(kf.env, main="K Function Plot (Skyrise Greenery)", xlim=c(0, 150))

## l-function
lf.env <- envelope(as.ppp(sf_skyrise_greenery$geometry),Lest,correction="border")
lf <- Lest(as.ppp(sf_skyrise_greenery$geometry), main=NULL,correction="border")
plot(lf.env, main = "L Function Envelop (Skyrise Greenery", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0.2, 0)))
plot(lf, . -r ~ r, main="", las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(0.2, 0)), xlab="", ylab="", xlim=c(0,100))
title(main = "L Function (Skyrise Greenery)", x = "r", y = "L(r)")

## g-function
gf <- Gest(as.ppp(sf_skyrise_greenery$geometry), main=NULL,correction="border")
gf.env <- envelope(as.ppp(sf_skyrise_greenery$geometry),Gest,correction="border")
plot(gf.env, main = "g Function (Skyrise Greenery)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)))
plot(gf.env, main = "g Function (Skyrise Greenery)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)), xlim=c(0, 150))
