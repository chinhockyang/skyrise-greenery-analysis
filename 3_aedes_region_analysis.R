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

# -------------- EDA: Skyrise HDB that are within Aedes Region --------------

sf_skyrise_hdb_within_aedes_regions <- st_join(st_buffer(sf_skyrise_hdb, 100), sf_aedes_regions, st_intersects) %>% 
                                            filter(!is.na(desc))

sf_skyrise_hdb_within_aedes_regions <- sf_skyrise_hdb_within_aedes_regions[c("ADDRESS", "total_dwelling_units", "max_floor_lvl", "year_completed", "type")]
row.names(sf_skyrise_hdb_within_aedes_regions) <- NULL
sf_skyrise_hdb_within_aedes_regions <- sf_skyrise_hdb_within_aedes_regions[!duplicated(sf_skyrise_hdb_within_aedes_regions),]

sf_skyrise_hdb$near_aedes_region <- apply(sf_skyrise_hdb, MARGIN=1, FUN=function(x) {
  if(x["ADDRESS"] %in% sf_skyrise_hdb_within_aedes_regions$ADDRESS) {
    return(1)
  } else {
    return(0)
  }
})

# Proportion of Buildings Near/Not Near Aedes Population
sf_skyrise_hdb_near_aedes_grouped <- sf_skyrise_hdb %>% group_by(type) %>% summarise(count=sum(near_aedes_region))
sf_skyrise_hdb_not_near_aedes_grouped <- sf_skyrise_hdb %>% group_by(type) %>% summarise(count=n_distinct(POSTAL_CODE) - sum(near_aedes_region))
sf_skyrise_hdb_near_aedes_grouped$label <- "Near Aedes Hotspot"
sf_skyrise_hdb_not_near_aedes_grouped$label <- "Not Near Aedes Hotspot"

ggplot(rbind(sf_skyrise_hdb_near_aedes_grouped, sf_skyrise_hdb_not_near_aedes_grouped)) +
  geom_bar(aes(x=type, y=count, fill=label), position="dodge", stat="identity")


# Locations of HDB near Aedes and Not-Near Aedes Regions
tm_shape(sf_skyrise_hdb) + 
  tm_symbols(col="near_aedes_region", size=0.6, palette="RdYlGn", border.alpha=1)


# Attribute Differences btwn Residential HDB near Aedes and Not-Near Aedes Regions
# --- 1. Total Dwelling Units
# --- 2. Floor Level
# --- 3. Year Completed
# [KIV]: Do analysis on them
sf_skyrise_hdb %>% filter(type=="Residential") %>%
        group_by(near_aedes_region) %>%
        summarise(
          mean(total_dwelling_units),
          mean(max_floor_lvl),
          mean(year_completed),
          n_distinct(POSTAL_CODE)
      )

# -------------- EDA: Aedes Hotspots --------------

sf_aedes_regions_centroid <- st_centroid(sf_aedes_regions)

# KDE
aedes_dens <- smooth_map(sf_aedes_regions_centroid, bandwidth = choose_bw(sf_aedes_regions_centroid$geometry))
tmap_mode('view')
tm_shape(aedes_dens$raster) + tm_raster() +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="near_aedes_region", size=0.6, palette="RdYlGn", border.alpha=1)

# Quadrat
Q_aedes <- quadratcount(as.ppp(sf_aedes_regions_centroid), nx=6, ny=3)
Q_aedes.d <- intensity(Q_aedes)
plot(intensity(Q_aedes, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(as.ppp(sf_aedes_regions_centroid), pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Area of each planning area
sf_planning_area$sqm <- st_area(sf_planning_area)
sf_planning_area$sqkm <- sf_planning_area$sqm / 1e06

# Number of Aedes Hotspot within subzone
sf_planning_area_w_aedes_hotspots <- st_join(sf_planning_area[c("pln_area", "sqkm")],
      st_join(sf_aedes_regions_centroid, sf_planning_area, st_within) %>% group_by(pln_area) %>% count(),
      left=TRUE
    )

# Preprocess Data
sf_planning_area_w_aedes_hotspots <- sf_planning_area_w_aedes_hotspots %>% 
                        rename(pln_area = pln_area.x) %>% dplyr::select(-pln_area.y) %>% 
                        replace_na(list(n = 0))

sf_planning_area_w_aedes_hotspots$aedes_hotspot_density <- sf_planning_area_w_aedes_hotspots$n / sf_planning_area_w_aedes_hotspots$sqkm

tm_shape(sf_planning_area_w_aedes_hotspots) +
  tm_polygons(col="aedes_hotspot_density", alpha=0.3) + tm_borders(alpha=0.9)


# -------------- EDA: Density/Distance EGDA on Skyrise HDB Near Aedes Hotspot --------------

# K-Function
kf_aedes <- Kest(as.ppp(sf_skyrise_hdb %>% filter(near_aedes_region == 1)), correction = 'border')
plot(kf_aedes, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf_aedes.env <- envelope(as.ppp(sf_skyrise_hdb %>% filter(near_aedes_region == 1)),Kest,correction="border")
plot(kf_aedes.env)

# L-Function
lf_aedes.env <- envelope(as.ppp(sf_skyrise_hdb %>% filter(near_aedes_region == 1)),Lest,correction="border")
lf_aedes <- Lest(as.ppp(sf_skyrise_hdb %>% filter(near_aedes_region == 1)), main=NULL,correction="border")
plot(lf_aedes.env)
plot(lf_aedes, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))


# -------------- Modified K-Function Calculation --------------

df <- data.frame()
for (i in seq(50,500,50)) {
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

K_df <- df %>% group_by(band) %>% summarise(K = mean(hotspot), events = n_distinct(ADDRESS))
K_aedes_plot <- rbind(
  data.frame(
    K = append(K_df$K * (as.numeric(st_area(sf_planning_area %>% st_union()), allow_mixed=TRUE) / 150), 0),
    band = append(K_df$band, 0),
    type = "K_hat"
  ),
  data.frame(
    K = append((K_df$band ** 2) * pi, 0),
    band = append(K_df$band, 0),
    type = "K_est"
  )
)

ggplot(K_aedes_plot, aes(x=band, y=K, group=type)) +
geom_line(aes(linetype=type)) + geom_point() +
scale_linetype_manual(values=c("dotted", "solid"))
