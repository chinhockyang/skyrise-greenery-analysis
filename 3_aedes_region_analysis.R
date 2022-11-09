
# Load Libraries and Variables
#===============================================================

# Data Files RequiredL
# 1. onemap_subzone (shp folder)
# 2. skyrise_greenery (shp folder)
# 3. addresses_full.csv (csv file)
# 4. high_aedes_population_regions (shp folder)
# 5. skyrise_hdb (shp folder)

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
library(stringr)
require(spatstat)
library(raster)
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
sf_skyrise_greenery$address <- apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  address = str_to_lower(x["address"])
  address = str_replace(address, "avenue", "ave")
  return(str_trim(str_replace(address, "singapore\\s*\\d*", "")))
})

# Load and Preprocess All Buildings Dataset  [skip to get to main analysis]
hdb_info <- read.csv("data/addresses_full.csv", colClasses=c("year_completed"="numeric", "max_floor_lvl"="numeric"))
hdb_info <- hdb_info %>% drop_na(LATITUDE)
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)
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
sf_skyrise_hdb <- sf_hdb %>% filter(
  (POSTAL_CODE %in% sf_skyrise_greenery$post_code) |
    (ADDRESS %in% sf_skyrise_greenery$address)
)
sf_hdb$greenery <- apply(sf_hdb, MARGIN = 1, FUN = function(x) {
  if ((x["POSTAL_CODE"] %in% sf_skyrise_hdb$POSTAL_CODE) | (x["ADDRESS"] %in% sf_skyrise_hdb$ADDRESS)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
sf_skyrise_greenery$greenery <- TRUE
sf_combinded_skyrise_greenery_and_hdb <- rbind(
  sf_hdb %>% filter(greenery == FALSE ) %>% dplyr::select(ADDRESS, POSTAL_CODE, greenery),
  sf_skyrise_greenery %>% dplyr::select(address, post_code, greenery) %>% dplyr::rename(ADDRESS = address, POSTAL_CODE = post_code)
)
sf_combinded_skyrise_greenery_and_hdb <- st_join(sf_combinded_skyrise_greenery_and_hdb, sf_planning_area, join = st_within)
row.names(sf_combinded_skyrise_greenery_and_hdb) <- NULL

# Classifying all buildings into 3 types:
# 1. HDB skyrise greenery
# 2. Non-HDB skyrise greenery
# 3. HDB non-skyrise greenery
external_skyrise_hdb_file <- st_as_sf(readOGR("data/skyrise_hdb"))
sf_combinded_skyrise_greenery_and_hdb$type <- apply(sf_combinded_skyrise_greenery_and_hdb, MARGIN=1, FUN=function(x) {
  if (x["ADDRESS"] %in% external_skyrise_hdb_file$ADDRESS_1) {
    return("HDB skyrise greenery")
  } else if (x["greenery"] == TRUE) {
    return("Non-HDB skyrise greenery")
  } else {
    return("HDB non-skyrise greenery")
  }
})


# Load Aedes Region Dataset
#===============================================================

aedes_regions <- readOGR("data/high_aedes_population_regions")
sf_aedes_regions <- st_as_sf(aedes_regions)
sf_aedes_regions <- st_transform(sf_aedes_regions, crs=3414)
head(sf_aedes_regions)


# Quick Plot to Visualise Locations of Aedes Hotspot and Skyrise Greenery
tmap_mode("view")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_skyrise_hdb) + tm_dots(col="red") +
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="green", alpha=0.5, size = 0.1) +
  tm_shape(sf_aedes_regions) + tm_fill(col="yellow", alpha = 0.7) + tm_borders(col="red", alpha=0.9) +
  tm_style(global.style) +
  tm_layout(
    title = "Aedes Hotspot and Skyrise Greenery", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# EDA on Area of Aedes Hotspot Polygons
sf_aedes_regions$area_sqkm <- as.numeric(st_area(sf_aedes_regions)) / 10^6
quantile(sf_aedes_regions$area_sqkm)
ggplot(sf_aedes_regions, aes(x=area_sqkm)) + geom_boxplot(fill="darkorange", alpha=0.5) +
  labs(y = "", x = "Area (sq km) of Aedes Hotspot")


# EDA on % of Skyrise Greenery and HDB within Aedes Hotspots
slices_greenery <- c(
  nrow(st_join(sf_skyrise_greenery, sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc))),
  nrow(sf_skyrise_greenery) - nrow(st_join(sf_skyrise_greenery, sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc)))
)
pie(slices_greenery, 
    labels=c(
      paste("Within Aedes Hotspot", slices_greenery[1], sep = " - "), 
      paste("Not Within Aedes Hotspot", slices_greenery[2], sep = " - ")
    ), 
    col=c("#e67c67", "#b8ffbc")
)

slices_non_greenery_hdb <- c(
  nrow(st_join(sf_hdb %>% filter(greenery==FALSE), sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc))),
  nrow(sf_hdb %>% filter(greenery==FALSE)) - nrow(st_join(sf_hdb %>% filter(greenery==FALSE), sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc)))
)
pie(slices_non_greenery_hdb, 
    labels=c(
      paste("Within Aedes Hotspot", slices_non_greenery_hdb[1], sep = " - "), 
      paste("Not Within Aedes Hotspot", slices_non_greenery_hdb[2], sep = " - ")
    ), 
    col=c("#e67c67", "#b8ffbc")
  )

slices_greenery_hdb <- c(
  nrow(st_join(sf_hdb %>% filter(greenery==TRUE), sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc))),
  nrow(sf_hdb %>% filter(greenery==TRUE)) - nrow(st_join(sf_hdb %>% filter(greenery==TRUE), sf_aedes_regions, st_within) %>% dplyr::filter(!is.na(desc)))
)
pie(slices_greenery_hdb, 
    labels=c(
      paste("Within Aedes Hotspot", slices_greenery_hdb[1], sep = " - "), 
      paste("Not Within Aedes Hotspot", slices_greenery_hdb[2], sep = " - ")
    ), 
    col=c("#e67c67", "#b8ffbc")
)



# Exploratory Spatial Data Analysis across Planning Area
#===============================================================

# Compute Area for each planning area
sf_planning_area$area <- as.numeric(st_area(sf_planning_area))

# Clipping Operation performed on each Aedes Hotspot (clip vector: planning area that Aedes Hotspot overlaps)
sf_aedes_region_in_each_planning_area <- st_intersection(sf_planning_area, sf_aedes_regions)

# Compute Aedes Hotspot Area for each Aedes Hotspot in each Planning Area
sf_aedes_region_in_each_planning_area$area <- as.numeric(st_area(sf_aedes_region_in_each_planning_area))

# Aggregate on Planning Area, Find Total Area of Aedes Hotspot in each Planning Area, and Number of Hotspot in the area
sf_aedes_across_planning_area <- sf_aedes_region_in_each_planning_area %>% group_by(pln_area) %>%
                                          summarise(
                                            total_aedes_area=sum(area), 
                                            num_of_hotspot=n_distinct(desc)
                                          )

# Combine Aggregated Data to original Planning Area dataset
sf_aedes_across_planning_area <- dplyr::left_join(sf_planning_area, st_drop_geometry(sf_aedes_across_planning_area))

# Compute Percentage of Planning Area Covered by Aedes Hotspot
sf_aedes_across_planning_area$pct_aedes_area <- (sf_aedes_across_planning_area$total_aedes_area / sf_aedes_across_planning_area$area) * 100


# Choropleth Visualisation - Number of Aedes Hotspot in each Planning Area
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_aedes_across_planning_area) + 
  tm_fill("num_of_hotspot", title="Aedes Hotspot") + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Number of Aedes Hotspot in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Pct of Land Covered by Aedes Hotspot across Planning Areas
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_aedes_across_planning_area) + 
  tm_fill("pct_aedes_area", title="Land Covered by Aedes Hotspot (%)") + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Percentage of Land that are Aedes Hotspot in each Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Skyrise Greenery's Distance from Nearest Hotspot Analysis
#===============================================================

# Identify Nearest Hotspot
nearest_hotspot <- st_nearest_feature(sf_combinded_skyrise_greenery_and_hdb, sf_aedes_regions)

# Compute Distance between Nearest Hotspot and All Buildings 
# [This code will take some time (10 min) to complete]
sf_combinded_skyrise_greenery_and_hdb$distance_from_nearest_hotspot <- NA
for (i in 1:nrow(sf_combinded_skyrise_greenery_and_hdb)) {
  building <- sf_combinded_skyrise_greenery_and_hdb[i,]
  nearest_aedes_hotspot <- sf_aedes_regions[nearest_hotspot[i],]
  sf_combinded_skyrise_greenery_and_hdb[i,"distance_from_nearest_hotspot"] <- as.numeric(st_distance(building, nearest_aedes_hotspot))
}

# Visualise one Example (for sanity check)
tmap_mode("view")
tm_shape(sf_combinded_skyrise_greenery_and_hdb[2,] %>% mutate(ADDRESS = str_to_title(ADDRESS))) + tm_symbols(col="darkgreen", size=0.0003) +
  tm_text("ADDRESS", col="darkgreen", just="top", ymod=0.001, size = 1) +
  tm_shape(sf_aedes_regions) + tm_fill(col="orange", alpha = 0.5) + tm_borders("red") + 
  tm_shape(sf_aedes_regions[nearest_hotspot[2],]) + tm_fill(col="red", alpha = 0.8) + tm_borders("yellow") +
  tm_text("desc", col="red", just="right", xmod=2.25, size = 1)


# Box Plot Visualisation
sf_combinded_skyrise_greenery_and_hdb %>%
  ggplot( aes(x=distance_from_nearest_hotspot, y=type, fill=greenery)) +
  geom_boxplot(outlier.shape = 2, outlier.size = 1) +
  scale_fill_manual(values=c("#e67c67", "#b8ffbc")) +
  xlab("Distance from Aedes Hotspot (m)") +
  ylab("") +
  theme_light()

# Density Plot Visualisation (HDB skyrise greenery vs HDB non-skyrise greenery)
sf_combinded_skyrise_greenery_and_hdb %>% filter(type != "Non-HDB skyrise greenery") %>%
  ggplot( aes(x=distance_from_nearest_hotspot, group=greenery, fill=greenery)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("#e67c67", "#b8ffbc")) +
  xlab("Distance from Aedes Hotspot (m)") +
  ylab("Density") +
  theme_light()


# Density Plot Visualisation (HDB skyrise greenery vs HDB non-skyrise greenery)
sf_combinded_skyrise_greenery_and_hdb %>% filter(type != "HDB non-skyrise greenery") %>%
  ggplot( aes(x=distance_from_nearest_hotspot, group=type, fill=type)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("darkgreen", "#b8ffbc")) +
  xlab("Distance from Aedes Hotspot (m)") +
  ylab("Density") +
  theme_light()


# Aggregating across Planning Area, Join with Planning Area dataset
sf_dist_from_aedes_agggregated <- sf_combinded_skyrise_greenery_and_hdb %>% group_by(pln_area) %>% 
  summarise(mean_dist = mean(distance_from_nearest_hotspot))

sf_aedes_across_planning_area <- dplyr::left_join(sf_aedes_across_planning_area, sf_dist_from_aedes_agggregated %>% st_drop_geometry())
# Export for Hypothesis Testing
sf_aedes_across_planning_area<- sf_aedes_across_planning_area %>% rename(
                                    aedes_area = total_aedes_area,
                                    hotspots = num_of_hotspot,
                                    aedespct = pct_aedes_area
                                    )
# st_write(sf_aedes_across_planning_area, "aedes_info_across_planning_area.shp", delete_layer = T)


# Aggregating across Planning Area and Greenery, Join with Planning Area dataset
sf_dist_from_aedes_agggregated_split_greenery <- sf_combinded_skyrise_greenery_and_hdb %>% group_by(pln_area, greenery) %>% 
                                          summarise(mean_dist = mean(distance_from_nearest_hotspot))
sf_dist_from_aedes_planning_area <- st_join(sf_planning_area,
                                            sf_dist_from_aedes_agggregated_split_greenery,
        join = st_contains) %>% dplyr::select(-pln_area.y) %>% rename(pln_area = pln_area.x)


# Compare Nearest Distance across planning areas with both Greenery and Non-Greenery buildings
sf_pln_area_w_both <- sf_dist_from_aedes_planning_area %>% group_by(pln_area) %>% summarise(n_distinct_greenery = n_distinct(greenery)) %>% filter(n_distinct_greenery > 1)
sf_dist_from_aedea_pln_area_w_both <- sf_dist_from_aedes_planning_area %>% filter(pln_area %in% sf_pln_area_w_both$pln_area)
sf_diff_in_dist_from_aedes_pln_area_w_both  <- sf_dist_from_aedea_pln_area_w_both %>% 
                                                      group_by(pln_area) %>% 
                                                      mutate(greenery_minus_non_greenery_mean_dist = mean_dist - lag(mean_dist)) %>% 
                                                      filter(greenery==TRUE) %>% 
                                                      dplyr::select(pln_area, greenery_minus_non_greenery_mean_dist) %>%
                                                      mutate(greenery_is_neaerer_to_aedes = greenery_minus_non_greenery_mean_dist < 0)
                    

# Choropleth Visualisation - All Buildings' Distance from Hotspot across Planning Areas
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_dist_from_aedes_planning_area %>% filter(mean_dist < 2500)) + 
  tm_fill("mean_dist", alpha=0.3, title="Mean Distance from Nearest Aedes Hotspot\n(< 2500m)", palette = "RdYlGn", breaks = seq(0,3000,200)) + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Distance of Buildings from Nearest Aedes Hotspot across Planning Areas", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Skyrise Greenery' Distance from Hotspot across Planning Areas
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_dist_from_aedes_planning_area %>% filter(mean_dist < 2500 & greenery == TRUE)) + 
  tm_fill("mean_dist", alpha=0.3, title="Mean Distance from Nearest Aedes Hotspot\n(< 2500m)", palette = "RdYlGn", breaks = seq(0,2600,200)) + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Distance of Skyrise Greenery from Nearest Aedes Hotspot across Planning Areas", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Non Skyrise Greenery' Distance from Hotspot across Planning Areas
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_dist_from_aedes_planning_area %>% filter(mean_dist < 2500 & greenery == FALSE)) + 
  tm_fill("mean_dist", alpha=0.3, title="Mean Distance from Nearest Aedes Hotspot\n(< 2500m)", palette = "RdYlGn", breaks = seq(0,2600,200)) + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Distance of Non-Skyrise Greenery from Nearest Aedes Hotspot across Planning Areas", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Comparing whether Skyrise Greenery is nearer to Aedes Hotspot across Planning Areas
tmap_mode('plot')
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
tm_shape(sf_diff_in_dist_from_aedes_pln_area_w_both) + 
  tm_fill("greenery_is_neaerer_to_aedes", alpha=0.3, title="Skyrise Greenery On Average Nearer to Aedes Hotspot", palette = "-RdYlGn") + tm_borders(alpha=0.9) +  
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1) +
  tm_layout(
    title = "Comparing if Skyrise Greenery is nearer to Aedes Hotspot\nacross Planning Areas (with both types)", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)



# Aedes Hotspots Centroid Point Pattern Analysis
#===============================================================

# Taking Centroid for each Hotspot Polygons
sf_aedes_regions_centroid <- st_centroid(sf_aedes_regions)


###--- Density-related Analysis ---###

# Quadrat
Q_aedes <- quadratcount(as.ppp(sf_aedes_regions_centroid), nx=8, ny=6)
plot(as.ppp(sf_aedes_regions_centroid$geometry), pch=20, cols="grey90", main=NULL)  # Plot points
plot(Q_aedes, add=TRUE)

# KDE
aedes_dens <- smooth_map(sf_aedes_regions_centroid, bandwidth = choose_bw(sf_aedes_regions_centroid$geometry))
tmap_mode('plot')
tm_shape(aedes_dens$raster) + tm_raster() +
  tm_shape(sf_planning_area) +tm_borders() +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# Hexagonal Binning
aedes_hex <- hexbin_map(as(sf_aedes_regions_centroid, "Spatial"), bins=25)
tmap_mode('plot')
tm_shape(aedes_hex) + tm_fill(col='z',title='Count',alpha=1) +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_legend(position=c("left", "top"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


###--- Distance-related Analysis ---###

# G-Function
gf_aedes <- Gest(as.ppp(sf_aedes_regions_centroid$geometry), main=NULL,correction="border")
gf_aedes.env <- envelope(as.ppp(sf_aedes_regions_centroid$geometry),Gest,correction="border")
plot(gf_aedes.env, main = "g Function (Aedes Hotspot Centroid)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)))
plot(gf_aedes.env, main = "g Function (Aedes Hotspot Centroid)", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0)), xlim=c(100, 300))

