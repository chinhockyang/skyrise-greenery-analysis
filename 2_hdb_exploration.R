# Exploration of HDB (Skyrise Greenery and other attributes)

# Load Libraries and Variables
#===============================================================

# Data Files RequiredL
# 1. onemap_subzone (shp folder)
# 2. skyrise_greenery (shp folder)
# 3. addresses_full.csv (csv file)

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
library(stringr)
require(spatstat)
library(oldtmaptools)
source("constants.R")

# Loading and Preparing Data used in earlier files 
# [skip this part to get to the main analysis codes]
#===============================================================
# Load and Preprocess Planning Area Dataset  [skip to get to main analysis]
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL
sf_subzone <- st_transform(sf_subzone, crs=3414)
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))

# Load and Preprocess Skyrise Greenery Dataset  [skip to get to main analysis]
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)
sf_skyrise_greenery$type <- unlist(apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  if (x["type"] %in% c("HDB", "MSCP", "Private Residential")) {
    return("Residential")
  } else {
    return(x["type"])
  }
}))



# Preparing External HDB dataset (Preprocessing)
#===============================================================
# Load Data
hdb_info <- read.csv("data/addresses_full.csv", colClasses=c("year_completed"="numeric", "max_floor_lvl"="numeric"))

# handle missing values
hdb_info <- hdb_info %>% drop_na(LATITUDE)

# Lowercase Address for Matching
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)

# Convert to SF
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

# Building Type Assignment
sf_hdb$type <- apply(sf_hdb, MARGIN=1, FUN=function(x) {
  if(x["residential"] == "Y") {
    return("Residential")
  } else if (x["multistorey_carpark"] == "Y") {
    return("MSCP")
  } else if (x["commercial"] == "Y") {
    return("Commercial")
  } else if (x["market_hawker"] == "Y") {
    return("Market/Hawker")
  } else if (x["precint_pavilion"] == "Y") {
    return("Pavillion")
  } else {
    return("Others")
  }
})


# EDA on Full HDB Dataset 
#===============================================================

# Number of HDB in dataset
nrow(sf_hdb)
dplyr::count(sf_hdb, type, sort=TRUE)


# Pie Chart of Types of HDB
ggplot(dplyr::count(sf_hdb, type, sort=TRUE), aes(x="", y = n, fill = type)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + scale_fill_brewer(palette = "Pastel1")  

# Dot Plot of HDB (and their Types)
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_hdb, title="Type") + tm_symbols(col="type", size=0.4) +
  tm_style(global.style) +
  tm_layout(
    title = "HDB Buildings (and their types)", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# HDB Skyrise Greenery Identification
#===============================================================

# lowercase address on Skyrise Greenery dataset and standardising texts
sf_skyrise_greenery$address <- apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  address = str_to_lower(x["address"])
  address = str_replace(address, "avenue", "ave")
  return(str_trim(str_replace(address, "singapore\\s*\\d*", "")))
})

# Merge by Postal Code or Address
sf_skyrise_hdb <- sf_hdb %>% filter(
    (POSTAL_CODE %in% sf_skyrise_greenery$post_code) |
    (ADDRESS %in% sf_skyrise_greenery$address)
)

# Create Boolean variable "greenery" to tell if building is skyrise greenery
sf_hdb$greenery <- apply(sf_hdb, MARGIN = 1, FUN = function(x) {
  if ((x["POSTAL_CODE"] %in% sf_skyrise_hdb$POSTAL_CODE) | (x["ADDRESS"] %in% sf_skyrise_hdb$ADDRESS)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# Combining All Unique Buildings from Both Datasets (use Address, Postal Code and greenery to filter and bind)
sf_skyrise_greenery$greenery <- TRUE
sf_combinded_skyrise_greenery_and_hdb <- rbind(
  sf_hdb %>% filter(greenery == FALSE ) %>% dplyr::select(ADDRESS, POSTAL_CODE, greenery),
  sf_skyrise_greenery %>% dplyr::select(address, post_code, greenery) %>% dplyr::rename(ADDRESS = address, POSTAL_CODE = post_code)
)
sf_combinded_skyrise_greenery_and_hdb <- st_join(sf_combinded_skyrise_greenery_and_hdb, sf_planning_area, join = st_within)
row.names(sf_combinded_skyrise_greenery_and_hdb) <- NULL


# Skyrise Greenery Exploratory Spatial Data Analysis
#===============================================================

# Locations of Skyrise HDB Buildings
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_skyrise_hdb, title="Type") + tm_symbols(col="type") +
  tm_style(global.style) +
  tm_layout(
    title = "HDB Skyrise Greenery in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Bar Chart of Types of HDB Skyrise Greenery
ggplot(dplyr::count(sf_skyrise_hdb, type, sort=TRUE), aes(x="", y = n, fill = type)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + scale_fill_brewer(palette = "Pastel1")


# Aggregate Data By Planning Area
# Look into mean Floor level, median Year Completed, number of buildings in each planning area
sf_hdb_by_planning_area <- st_join(sf_hdb, sf_planning_area, join = st_within)
sf_hdb_group_by_planning_area <- sf_hdb_by_planning_area %>% 
                              filter(residential == "Y") %>% 
                              group_by(pln_area) %>%
                              summarise(
                                mean_floor_lvl = mean(max_floor_lvl),
                                median_year_completed = median(year_completed, na.rm=FALSE),
                                count = n_distinct(POSTAL_CODE)
                              )

sf_hdb_planning_area_info <- st_join(sf_planning_area,
                                    sf_hdb_group_by_planning_area,
                                    join = st_contains) %>% replace_na(list(count = 0))

# Choropleth Visualisation - HDB across Planning Areas
tmap_mode('plot')
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("count", alpha=1, title="Number of Buildings") + tm_borders(alpha=0.9) +  
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Number of HDB across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Year Completion
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("median_year_completed", alpha=1, title="Year Completed (Median)") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Year of Completion of HDB in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Floor Level
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("mean_floor_lvl", alpha=1, title="Floor Level") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Floor Levels of HDB across Planning Areas", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# export out for use in hypo testing
# sf_hdb_planning_area_info <- sf_hdb_planning_area_info %>% dplyr::select(pln_area.x, geometry, mean_floor_lvl, median_year_completed, count) %>%
  # dplyr::rename(pln_area = pln_area.x, floor = mean_floor_lvl, year = median_year_completed)
# st_write(sf_hdb_planning_area_info, "hdb_info_accross_planning_area.shp", delete_layer = T)


# Point Pattern Analysis
#===============================================================
# -------------- All HDB -------------- #

###--- Density-related Analysis ---###

# Quadrat
Q_hdb <- quadratcount(as.ppp(sf_hdb), nx=8, ny=6)
plot(as.ppp(sf_hdb$geometry), pch=20, cols="grey90", main=NULL)  # Plot points
plot(Q_hdb, add=TRUE)
Q_hdb.d <- intensity(Q_hdb)
plot(intensity(Q_hdb, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(as.ppp(sf_hdb$geometry), pch=20, cex=1, cols="grey70", add=TRUE)  # Add points

# KDE
hdb_dens <- smooth_map(sf_hdb, bandwidth = choose_bw(sf_hdb$geometry))
tmap_mode('plot')
tm_shape(hdb_dens$raster) + tm_raster() +
  tm_shape(sf_planning_area) +tm_borders() +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# Hexagonal Binning
hdb_hex <- hexbin_map(as(sf_hdb, "Spatial"), bins=25)
tm_shape(hdb_hex) + tm_fill(col='z',title='Count',alpha=1) +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


###--- Distance-related Analysis ---###

# K-Function
kf_hdb <- Kest(as.ppp(sf_hdb$geometry), correction = 'border')
plot(kf_hdb, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf_hdb.env <- envelope(as.ppp(sf_hdb$geometry),Kest,correction="border")
plot(kf_hdb.env)

# L-Function
lf_hdb.env <- envelope(as.ppp(sf_hdb$geometry),Lest,correction="border")
lf_hdb <- Lest(as.ppp(sf_hdb$geometry), main=NULL,correction="border")
plot(lf_hdb.env)
plot(lf_hdb, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))

# G-Function
gf_hdb <- Gest(as.ppp(sf_hdb$geometry), main=NULL,correction="border")
gf_hdb.env <- envelope(as.ppp(sf_hdb$geometry),Gest,correction="border")
plot(gf_hdb.env, main = "g Function (HDB)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)))
plot(gf_hdb.env, main = "g Function (HDB)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)), xlim=c(0, 40))

# -------------- Skyrise HDB -------------- #

###--- Density-related Analysis ---###

# Quadrat
Q_skyrise_hdb <- quadratcount(as.ppp(sf_skyrise_hdb), nx=8, ny=6)
plot(as.ppp(sf_skyrise_hdb$geometry), pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q_skyrise_hdb, add=TRUE)

# KDE
skyrise_hdb_dens <- smooth_map(sf_skyrise_hdb, bandwidth = choose_bw(sf_skyrise_hdb$geometry))
tmap_mode('plot')
tm_shape(skyrise_hdb_dens$raster) + tm_raster() +
  tm_shape(sf_planning_area) +tm_borders() +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# Hex Binning
skyrise_hdb_hex <- hexbin_map(as(sf_skyrise_hdb, "Spatial"), bins=25)
tm_shape(skyrise_hdb_hex) + tm_fill(col='z',title='Count',alpha=1) +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


###--- Distance-related Analysis ---###

# K-Function
kf <- Kest(as.ppp(sf_skyrise_hdb$geometry), correction = 'border')
plot(kf, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Kest,correction="border")
plot(kf.env)

# L-Function
lf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Lest,correction="border")
lf <- Lest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
plot(lf.env)
plot(lf, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))

# G-Function
gf_skyrise_hdb <- Gest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
gf_skyrise_hdb.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Gest,correction="border")
plot(gf_skyrise_hdb.env)

plot(gf_skyrise_hdb.env, main = "g Function (Skyrise Greenery HDB)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)))
plot(gf_skyrise_hdb.env, main = "g Function (Skyrise Greenery HDB)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)), xlim=c(0, 80))

