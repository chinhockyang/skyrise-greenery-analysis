# Skyrise Greenery Regression   
# -------------- Load Libraries --------------
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

#-------------- Prepare Data --------------------
# -------------- Prepare HDB dataset --------------
hdb_info <- read.csv("data/hdb_data/addresses_full.csv")

# handle the 2 missing values
hdb_info <- hdb_info %>% drop_na(LATITUDE)

# Lowercase Address for Matching
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)

# # Convert to Sf
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

# -------------- Identify HDB that are Skyrise Greenery --------------
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_make_valid(st_as_sf(skyrise_greenery))
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs=3414)

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

#-------------- Join Skyrise Greenery HDB Dataset with resale prices -----------------
# Read HDB Prices Dataset
hdb_prices <- read.csv('data/hdb_data/hdb_add_with_prices_only.csv')

# Aggregate resale prices for all records with the same postal code & get average price of a HDB block
hdb_average_prices <- hdb_prices %>% 
                          group_by(POSTAL_CODE) %>% 
                              summarise_at(vars(RESALE_PRICE), list(name = mean))

# Merge by Postal Code 
skyrise_hdb_with_prices <- merge(x = sf_skyrise_hdb, y = hdb_average_prices, by = "POSTAL_CODE", x.all=TRUE)

############# Skipped AutoCorrelation check for skyrise HDB prices #############

# -------------- Load subzone dataset --------------
# Load Dataset
sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL

# Convert to EPSG:3414
sf_subzone <- st_transform(sf_subzone, crs=3414)

# Create Planning Area Polygons
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))

# Visualise the subzones
plot(sf_planning_area$geometry) 

# Obtain area of each subzone
sf_subzone$area <- st_area(sf_subzone)
sf_planning_area$area <- st_area(sf_planning_area)

# Convert to kilometers^2 instead so that the values are larger 
sf_subzone$area_km2 <- sf_subzone$area / 10**6
sf_planning_area$area_km2 <- sf_planning_area$area / 10**6

# Density of skyrise greenery = number of skyrise greenery in a subzone / area 
# Check that both SPDFs have the same CRS
st_crs(sf_skyrise_hdb)
st_crs(sf_subzone)

skyrise_subzone_join <- st_join(sf_skyrise_hdb, 
                                sf_subzone, 
                                join = st_within, 
                                left = TRUE)  %>% 
                                              dplyr::select("ADDRESS", "subzone", "area", "geometry") %>%
                                              group_by(subzone) %>% count()

sf_subzone_w_skyrise_greenery_info <- st_join(sf_subzone, 
                                              skyrise_subzone_join, 
                                              join = st_contains) 

# sf_subzone_w_skyrise_greenery_info[is.na(sf_subzone_w_skyrise_greenery_info)] <- 0

sf_subzone_w_skyrise_greenery_info <- subset(sf_subzone_w_skyrise_greenery_info, select=-c(subzone.y, area))

sf_subzone_w_skyrise_greenery_info$density <- sf_subzone_w_skyrise_greenery_info$n / sf_subzone_w_skyrise_greenery_info$area_km2

hist(sf_subzone_w_skyrise_greenery_info$density, main=NULL)

# Choropleth Chart showing Number of Skyrise Greenery HDBs across Subzones
tmap_mode('view')
tm_shape(sf_subzone_w_skyrise_greenery_info) + 
  tm_fill("n", alpha=1, title="Number of Skyrise HDB") + tm_borders(alpha=0.9)

# Choropleth Chart showing Density of Skyrise Greenery HDBs across Subzones
tm_shape(sf_subzone_w_skyrise_greenery_info) + 
  tm_fill("density", alpha=1, title="Density of Skyrise HDB Across Subzones") + tm_borders(alpha=0.9)

tm_shape(sf_planning_area) + 
  tm_fill("pln_area", alpha=1, title="Planning Area in Singapore") + tm_borders(alpha=0.9)

#################### PLANNING AREA LEVEL ############################
skyrise_pln_area_join <- st_join(sf_skyrise_hdb, 
                             sf_planning_area, 
                             join = st_within, 
                             left = TRUE)  %>% dplyr::select("ADDRESS", "pln_area", "area", "geometry") %>%
                                                        group_by(pln_area) %>% count()

sf_pln_area_w_skyrise_greenery_info <- st_join(sf_planning_area, 
                                              skyrise_pln_area_join, 
                                              join = st_contains) 

# sf_pln_area_w_skyrise_greenery_info[is.na(sf_pln_area_w_skyrise_greenery_info)] <- 0

sf_pln_area_w_skyrise_greenery_info <- subset(sf_pln_area_w_skyrise_greenery_info, select=-c(pln_area.y, area))

sf_pln_area_w_skyrise_greenery_info$density <- sf_pln_area_w_skyrise_greenery_info$n / sf_pln_area_w_skyrise_greenery_info$area_km2

hist(sf_pln_area_w_skyrise_greenery_info$density, main=NULL)

# Choropleth Chart showing Number of Skyrise Greenery HDBs across PLN AREA
tmap_mode('view')
tm_shape(sf_pln_area_w_skyrise_greenery_info) + 
  tm_fill("density", alpha=1, title="Density of Skyrise HDB") + tm_borders(alpha=0.9)

tm_shape(sf_pln_area_w_skyrise_greenery_info) + 
  tm_fill("n", alpha=1, title="Number of Skyrise HDB") + tm_borders(alpha=0.9)

#1. Visual exploration of autocorrelation
#===============================================================
# Subzone Level
sp_subzone_skyrise_info <- sf:::as_Spatial(sf_subzone_w_skyrise_greenery_info)
sp_subzone_skyrise_info <- subset(sp_subzone_skyrise_info, select=c(subzone.x, n, density))
sp_subzone_skyrise_info.utm <-spTransform(sp_subzone_skyrise_info, CRS("+init=epsg:3414"))

# Planning Area Level
sp_pln_area_skyrise_info <- sf:::as_Spatial(sf_pln_area_w_skyrise_greenery_info)
sp_pln_area_skyrise_info <- subset(sp_pln_area_skyrise_info, select=c(pln_area.x, n, density))
sp_pln_area_skyrise_info.utm <-spTransform(sp_pln_area_skyrise_info, CRS("+init=epsg:3414"))

# Draw a choropleth map of the HDB density (SUBZONE)
tm_shape(sp_subzone_skyrise_info.utm) + tm_polygons(col='density',title= 'Density of HDBs')

# Draw a choropleth map of the HDB density (PLN AREA)
tm_shape(sp_pln_area_skyrise_info.utm) + tm_polygons(col='density',title= 'Density of HDBs (Planning Area)')

#2.	Neighbors and Lagged Mean Plots
#===============================================================
require(spdep)
#---------------------- Pre-processing ------------------------
# Subzone Level
# Drop islands without neighbours: Semakau, Sudong, North-Eastern Islands, Southern Group, Jurong Island and Bukom, Changi Bay, Tuas View Extension, Murai
sp_subzone_skyrise_info.utm <- sp_subzone_skyrise_info.utm[-c(8,4,7,9,304,159,332,300), ]

# Visualise after dropping islands without neighbours
tm_shape(sp_subzone_skyrise_info.utm) + tm_polygons(col='n',title= 'Num of HDBs')

# Planning Area Level
# drop western islands, southern islands, western water catchment, simpang, north-eastern islands, changi bay
sp_pln_area_skyrise_info.utm <- sp_pln_area_skyrise_info.utm[-c(52, 53, 44, 42, 27, 11), ]

# Visualise after dropping islands without neighbours
tm_shape(sp_pln_area_skyrise_info.utm) + tm_polygons(col='n',title= 'Num of HDBs')

#---------------------- Queen’s case neighbours ------------------------
# Subzone Level
sp_subzone_skyrise_info.nb <- poly2nb(sp_subzone_skyrise_info.utm)
sp_subzone_skyrise_info.nb

# Convert the neighbour list to a listw object
sp_subzone_skyrise_info.lw <- nb2listw(sp_subzone_skyrise_info.nb, zero.policy = TRUE)
# sp_subzone_skyrise_info.lw
sp_subzone_skyrise_info.utm$n.lagged.means <- lag.listw(sp_subzone_skyrise_info.lw,sp_subzone_skyrise_info.utm$n)

tm_shape(sp_subzone_skyrise_info.utm) + tm_polygons(col= 'n.lagged.means',title= 'Num of HDBs (Queen)') +
  tm_layout(legend.bg.color = "white")

# Planning Area Level
sp_pln_area_skyrise_info.nb <- poly2nb(sp_pln_area_skyrise_info.utm)
sp_pln_area_skyrise_info.nb

# Convert the neighbour list to a listw object
sp_pln_area_skyrise_info.lw <- nb2listw(sp_pln_area_skyrise_info.nb, zero.policy = TRUE)
# sp_subzone_skyrise_info.lw
sp_pln_area_skyrise_info.nb$n.lagged.means <- lag.listw(sp_pln_area_skyrise_info.lw,sp_pln_area_skyrise_info.nb$n)

tm_shape(sp_pln_area_skyrise_info.utm) + tm_polygons(col= 'n.lagged.means',title= 'Num of HDBs (Queen)') +
  tm_layout(legend.bg.color = "white")

#Rook's case neighbours
#---------------------------------------------------------------
# Calculate the Rook's case neighbours
sp_subzone_skyrise_info.nb2 <- poly2nb(sp_subzone_skyrise_info.utm, queen=FALSE)

# Convert the neighbour list to a listw object - use Rook's case...
sp_subzone_skyrise_info.lw2 <- nb2listw(sp_subzone_skyrise_info.nb2, zero.policy = TRUE)
# sp_subzone_skyrise_info.lw2
sp_subzone_skyrise_info.utm$n.lagged.means2 <- lag.listw(sp_subzone_skyrise_info.lw2,sp_subzone_skyrise_info.utm$n)

tm_shape(sp_subzone_skyrise_info.utm) + tm_polygons(col= 'n.lagged.means2',title= 'Num of HDBs (Rook)') +
  tm_layout(legend.bg.color = "white")

# Computing Moran’s I
#===============================================================
moran.test(sp_subzone_skyrise_info.utm$n,sp_subzone_skyrise_info.lw,randomisation=FALSE, zero.policy = TRUE)

# Simulation-based approach
#===============================================================
mc <- moran.mc(sp_subzone_skyrise_info.utm$n,sp_subzone_skyrise_info.lw,10000,zero.policy = TRUE)

plot(mc)

## plotting the relationship between number of skyrise greenery and its spatially lagged counterpart 
plot(sp_pln_area_skyrise_info.nb$n.lagged.means ~ sp_subzone_skyrise_info.utm, pch=16, asp=1)
lm1 <- lm(sp_pln_area_skyrise_info.nb$n.lagged.means ~ sp_subzone_skyrise_info.utm)
abline(lm1, col='blue')

#The function of Local Moran's I  
# Mapping Local Moran's I
tmap_mode('plot')
sp_subzone_skyrise_info.utm$lI <- localmoran(sp_subzone_skyrise_info.utm$n,sp_subzone_skyrise_info.lw, zero.policy = TRUE)[, 1]
sp_subzone_skyrise_info.utm$lI2 <- localmoran(sp_subzone_skyrise_info.utm$n,sp_subzone_skyrise_info.lw2, zero.policy = TRUE)[, 1]

# for queen
tm_shape(sp_subzone_skyrise_info.utm,unit='miles') + 
  tm_polygons(col= 'lI',title= "Local Moran's I (queen)",legend.format=list(flag= "+"),midpoint = NA) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.6, asp=1.8)

# for rook
tm_shape(sp_subzone_skyrise_info.utm,unit='miles') + 
  tm_polygons(col= 'lI2',title= "Local Moran's I (rook)",legend.format=list(flag= "+"),midpoint = NA) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.6, asp=1.8)

#Mapping the p values of Local Moran's I  
#---------------------------------------------------------------
sp_subzone_skyrise_info.utm$pval <- localmoran(sp_subzone_skyrise_info.utm$n,sp_subzone_skyrise_info.lw, zero.policy = TRUE)[, 5]

tmap_mode("view")

# Draw the map
tm_shape(sp_subzone_skyrise_info.utm,unit= 'miles') +
  tm_polygons(col= 'pval' , title= "p-value" , breaks= c(0, 0.01, 0.05, 0.10, 1),
              border.col = "black",palette = "-Greens") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8)


# SAR model with/without predictor variable
#==============================================================

#SAR model without predictor variable
#---------------------------------------------------------------
sar.res <- spautolm(n~ 1, listw=sp_subzone_skyrise_info.lw, data=sp_subzone_skyrise_info.utm)
sar.res

summary(sar.res)

#Estimation of standard error of I  
#---------------------------------------------------------------
sar.res$lambda.se

sar.res$lambda + c(-2,2)*sar.res$lambda.se


