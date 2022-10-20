# -------------- Load Libraries and Variables --------------

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
# tmaptools::palette_explorer()

library(stringr)
require(spatstat)
library(raster)

source("constants.R")
source("1_initial_exploration.R")
source("2_hdb_exploration.R")

# -------------- Load Aedes Region Dataset --------------

aedes_regions <- readOGR("data/high_aedes_population_regions")
sf_aedes_regions <- st_as_sf(aedes_regions)
sf_aedes_regions <- st_transform(sf_aedes_regions, crs=3414)
head(sf_aedes_regions)

qtm(sf_aedes_regions)

# -------------- Centroid of high Aedes population region --------------

sf_aedes_regions_centroid <- st_centroid(sf_aedes_regions)

aedes_dens <- smooth_map(sf_aedes_regions, bandwidth = choose_bw(sf_aedes_regions$geometry))

tmap_mode('view')
tm_shape(aedes_dens$raster) + tm_raster() +
  tm_shape(sf_subzone) +tm_borders(alpha=0.)

# -------------- Density of Aedes Hotspots in Subzone --------------

# Area of each subzone
sf_subzone$sqm <- st_area(sf_subzone)
sf_subzone$sqkm <- sf_subzone$sqm / 1e06

# Number of Aedes Hotspot within subzone
sf_subzone_w_aedes_hotspots <- st_join(sf_subzone[c("pln_area", "region", "sqkm", "subzone")],
      st_join(sf_aedes_regions_centroid, sf_subzone, st_within) %>% group_by(subzone) %>% count(),
      left=TRUE
    )

# Preprocess Data
sf_subzone_w_aedes_hotspots <- sf_subzone_w_aedes_hotspots %>% 
                        rename(subzone = subzone.x) %>% dplyr::select(-subzone.y) %>% 
                        replace_na(list(n = 0)) 

sf_subzone_w_aedes_hotspots$aedes_hotspot_density <- sf_subzone_w_aedes_hotspots$n / sf_subzone_w_aedes_hotspots$sqkm

tm_shape(sf_subzone_w_aedes_hotspots) +
  tm_polygons(col="aedes_hotspot_density", alpha=0.3) + tm_borders(alpha=0.9)


# -------------- Manual K-Function Calculation --------------


df <- data.frame()
for (i in seq(0,500,50)) {
  # Create a Buffer around the HDB
  sf_skyrise_hdb_buffer <- st_buffer(sf_skyrise_hdb, i)
  skyrise_with_nearby_hotspot <- st_join(sf_skyrise_hdb_buffer, sf_aedes_regions_centroid, st_contains) %>% filter(!is.na(desc)) %>% group_by(ADDRESS) %>% summarise(hotspot = n_distinct(desc))
  skyrise_without_nearby_hotspot <- sf_skyrise_hdb_buffer %>% filter(!(ADDRESS %in% skyrise_with_nearby_hotspot$ADDRESS))
  skyrise_without_nearby_hotspot <- skyrise_without_nearby_hotspot["ADDRESS"]
  skyrise_without_nearby_hotspot["hotspot"] <- 0
  
  combined <- rbind(skyrise_with_nearby_hotspot, skyrise_without_nearby_hotspot)
  combined$band <- i
  df <- rbind(df, combined)
}


print(df %>% filter(band == 150))
K_df <- df %>% group_by(band) %>% summarise(K = mean(hotspot), events = n_distinct(ADDRESS))
print(K_df)
plot(K_df$band,K_df$K)