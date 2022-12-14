# Skyrise Greenery Spatial autocorrelation   (All Skyrise Greenery)

# Load Libraries and Variables
#===============================================================

# Data Files Required
# 1. onemap_subzone (shp folder)
# 2. skyrise_greenery (shp folder)
# 3. hdb_add_with_prices_only (csv)

library(tmap)
library(tmaptools)
library(GISTools)
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(spdep)
library(dplyr)
library(maptools)
library(sp)
library(spatialreg)

source("constants.R")

# Importing Datasets
#===============================================================

# 1. Planning Area Island Shape
#===============================================================
# Load Dataset
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL

# Convert to EPSG:3414
sf_subzone <- st_transform(sf_subzone, crs=3414)

# Create Planning Area Polygons
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))

# Obtain area of each subzone / planning area
sf_subzone$area <- st_area(sf_subzone)
sf_planning_area$area <- st_area(sf_planning_area)

# Convert to kilometers^2 instead so that the values are larger 
sf_subzone$area_km2 <- sf_subzone$area / 10**6
sf_planning_area$area_km2 <- sf_planning_area$area / 10**6

# 2. HDB Resale Prices
#===============================================================
# Import csv file 
hdb_prices <- read.csv('data/hdb_add_with_prices_only.csv')

# Aggregate resale prices for all records with the same postal code & 
# get average price of a HDB block
hdb_average_prices <- hdb_prices %>% 
  group_by(POSTAL_CODE) %>% 
  summarise_at(vars(RESALE_PRICE), list(name = mean))

# Convert to Sf
sf_hdb_prices <- st_as_sf(hdb_prices, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb_prices <- st_transform(sf_hdb_prices, crs= 3414)


# 3. Skyrise Greenery Dataset (General)
#===============================================================
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)


# 4. Skyrise Greenery Dataset (HDB only)
#===============================================================
hdb_info <- read.csv("data/addresses_full.csv") # read dataset
hdb_info <- hdb_info %>% drop_na(LATITUDE) # handle the 2 missing values
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS) # lowercase Address for Matching

# Convert to Sf
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

# lowercase address
sf_skyrise_greenery$address <- apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  address = str_to_lower(x["address"])
  address = str_replace(address, "avenue", "ave")
  return(str_trim(str_replace(address, "singapore\\s*\\d*", "")))
})

# Merge by Postal Code or Address
# 150
sf_skyrise_hdb <- sf_hdb %>% filter(
  (POSTAL_CODE %in% sf_skyrise_greenery$post_code) |
    (ADDRESS %in% sf_skyrise_greenery$address)
)

# Exploration of all skyrise greenery buildings 
#===============================================================
all_skyrise_pln_area_join <- st_join(sf_skyrise_greenery, 
                                     sf_planning_area, 
                                     join = st_within, 
                                     left = TRUE)  %>% 
  dplyr::select("pln_area", "area", "geometry") %>%
  group_by(pln_area) %>% count()

all_skyrise_pln_area_join <- st_join(sf_planning_area, 
                                     all_skyrise_pln_area_join, 
                                     join = st_contains) 

# drop duplicate pln_area column
all_skyrise_pln_area_join <- subset(all_skyrise_pln_area_join, select=-c(pln_area.y))

# assign 0 to all pln_areas without skyrise greenery buildings
all_skyrise_pln_area_join[is.na(all_skyrise_pln_area_join)] <- 0

# create density column
# density of skyrise greenery = number of skyrise greenery in a subzone / area (in m^2)
all_skyrise_pln_area_join$density <- all_skyrise_pln_area_join$n / all_skyrise_pln_area_join$area

hist(all_skyrise_pln_area_join$density, main=NULL)

# Visualisations 
#===============================================================
# Choropleth Chart showing Density of Skyrise Greenery Buildings across Pln Areas
tm_shape(all_skyrise_pln_area_join) + 
  tm_fill("density", alpha=1) + 
  tm_borders(alpha=0.9) +
  tm_layout(main.title="Density of Skyrise Greenery Buildings Across Pln Areas",
            legend.position = c("right", "bottom"), legend.text.size= 0.65, asp=1.5, legend.height=0.35)

# Choropleth Chart showing Number of Skyrise Greenery Buildings across Pln Areas
tm_shape(all_skyrise_pln_area_join) + 
  tm_fill("n", alpha=1) + 
  tm_borders(alpha=0.9) +
  tm_layout(main.title="Number of Skyrise Greenery Buildings Across Pln Areas",
            legend.position = c("right", "bottom"), legend.text.size= 0.65, asp=1.5, legend.height=0.45)


# 1. Plot Randomly Generated Distributions of Skyrise Greenery 
#===============================================================
# Create .utm file
sp_pln_area_all_skyrise_info <- sf:::as_Spatial(all_skyrise_pln_area_join)
sp_pln_area_all_skyrise_info <- subset(sp_pln_area_all_skyrise_info, select=c(pln_area.x, n, density))
sp_pln_area_all_skyrise_info.utm <-spTransform(sp_pln_area_all_skyrise_info, CRS("+init=epsg:3414"))

#### DENSITY
set.seed(300)
sp_pln_area_all_skyrise_info.utm$density_rand1 <- sample(sp_pln_area_all_skyrise_info.utm$density) 
sp_pln_area_all_skyrise_info.utm$density_rand2 <- sample(sp_pln_area_all_skyrise_info.utm$density) 
sp_pln_area_all_skyrise_info.utm$density_rand3 <- sample(sp_pln_area_all_skyrise_info.utm$density) 

vars <- c('density', 'density_rand1', 'density_rand2', 'density_rand3')

tm_shape(sp_pln_area_all_skyrise_info.utm) + 
  tm_polygons(col=vars, legend.show=FALSE) + 
  tm_layout(title= 1:6, title.position= c("right","top"))

#### NUMBERS 
set.seed(300)
sp_pln_area_all_skyrise_info.utm$n_rand1 <- sample(sp_pln_area_all_skyrise_info.utm$n) 
sp_pln_area_all_skyrise_info.utm$n_rand2 <- sample(sp_pln_area_all_skyrise_info.utm$n) 
sp_pln_area_all_skyrise_info.utm$n_rand3 <- sample(sp_pln_area_all_skyrise_info.utm$n) 

vars2 <- c('n', 'n_rand1', 'n_rand2', 'n_rand3')

tm_shape(sp_pln_area_all_skyrise_info.utm) + 
  tm_polygons(col=vars2, legend.show=FALSE) + 
  tm_layout(title= 1:6, title.position= c("right","top"))

# Pre-Processing
# 1. Drop Planning Areas without neighbours
#===============================================================
# drop western islands, southern islands, western water catchment, 
# simpang, north-eastern islands, changi bay, tuas
sp_pln_area_all_skyrise_info.utm <- sp_pln_area_all_skyrise_info.utm[-c(51, 52, 53, 44, 42, 27, 11), ]

# Visualise after dropping islands without neighbours
tm_shape(sp_pln_area_all_skyrise_info.utm) + 
  tm_polygons(col='n',title= 'Number of Skyrise Greenery Buildings') + 
  tm_layout(legend.position=c("left", "top"))

# 2. Create Neighbour List 
#===============================================================
tmap_mode('view')
tmap_mode('plot')

# 2A. Queen's Case
#===============================================================
# Planning Area Level
sp_pln_area_all_skyrise_info.nb <- poly2nb(sp_pln_area_all_skyrise_info.utm)
sp_pln_area_all_skyrise_info.nb

# Convert the neighbour list to a listw object
sp_pln_area_all_skyrise_info.lw <- nb2listw(sp_pln_area_all_skyrise_info.nb, zero.policy = TRUE)
sp_pln_area_all_skyrise_info.lw
sp_pln_area_all_skyrise_info.utm$n.lagged.means <- lag.listw(sp_pln_area_all_skyrise_info.lw, sp_pln_area_all_skyrise_info.utm$n)

tm_shape(sp_pln_area_all_skyrise_info.utm) + 
  tm_polygons(col= 'n.lagged.means', 
              title= 'Num of Skyrise Greenery Buildings (Queen)') +
  tm_layout(legend.position = c("left", "top"))

# 2B. Rook's Case
#===============================================================
# Calculate the Rook's case neighbours
sp_pln_area_all_skyrise_info.nb2 <- poly2nb(sp_pln_area_all_skyrise_info.utm, queen=FALSE)

# Convert the neighbour list to a listw object - use Rook's case...
sp_pln_area_all_skyrise_info.lw2 <- nb2listw(sp_pln_area_all_skyrise_info.nb2, zero.policy = TRUE)
sp_pln_area_all_skyrise_info.lw2
sp_pln_area_all_skyrise_info.utm$n.lagged.means2 <- lag.listw(sp_pln_area_all_skyrise_info.lw2, sp_pln_area_all_skyrise_info.utm$n)

tm_shape(sp_pln_area_all_skyrise_info.utm) + tm_polygons(col= 'n.lagged.means2',title= 'Num of Skyrise Greenery Buildings (Rook)') +
  tm_layout(legend.position=c("left", "top"))

# 3. Computing Moran's I Coefficients
#===============================================================
# Moran's I Scatter Plot
moran.plot(sp_pln_area_all_skyrise_info.utm$n, sp_pln_area_all_skyrise_info.lw)

moran.test(sp_pln_area_all_skyrise_info.utm$n, 
           sp_pln_area_all_skyrise_info.lw,
           randomisation=FALSE, 
           zero.policy = TRUE)

# Simulation-based approach
#===============================================================
mc <- moran.mc(sp_pln_area_all_skyrise_info.utm$n,
               sp_pln_area_all_skyrise_info.lw,
               10000, zero.policy = TRUE)

plot(mc)

# The function of Local Moran's I
#===============================================================
# Mapping Local Moran's I
tmap_mode('view')
sp_pln_area_all_skyrise_info.utm$lI <- localmoran(sp_pln_area_all_skyrise_info.utm$n,
                                                  sp_pln_area_all_skyrise_info.lw, 
                                                  zero.policy = TRUE)[, 1]

# for queen
tm_shape(sp_pln_area_all_skyrise_info.utm,unit='miles') + 
  tm_polygons(col= 'lI',title= "Local Moran's I (Queen) - All Skyrise Greenery Buildings",legend.format=list(flag= "+"),midpoint = NA) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15) +
  tm_layout(legend.position = c("left", "top"), legend.text.size= 0.6, asp=1.8)


# Mapping the p values of Local Moran's I  
#===============================================================
sp_pln_area_all_skyrise_info.utm$pval <- localmoran(sp_pln_area_all_skyrise_info.utm$n,
                                                    sp_pln_area_all_skyrise_info.lw, 
                                                    zero.policy = TRUE)[, 5]

# Draw the map
tm_shape(sp_pln_area_all_skyrise_info.utm,unit= 'miles') +
  tm_polygons(col= 'pval' , title= "p-value" , breaks= c(0, 0.01, 0.05, 0.10, 1),
              border.col = "black",palette = "-Greens") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "top"),asp=1.8)

# Not used since there is no spatial autocorrelation in the data
# # 4. Conducting Spatial Autoregression
# #===============================================================
# #SAR model without predictor variable
# #---------------------------------------------------------------
# sar.res2 <- spautolm(n~ 1, listw=sp_pln_area_all_skyrise_info.lw, data=sp_pln_area_all_skyrise_info.utm)
# sar.res2
# 
# summary(sar.res2)
# 
# #Estimation of standard error of I  
# #---------------------------------------------------------------
# sar.res2$lambda.se
# 
# sar.res2$lambda + c(-2,2)*sar.res2$lambda.se


