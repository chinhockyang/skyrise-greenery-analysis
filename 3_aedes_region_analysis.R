
# Load Libraries and Variables
#===============================================================

library(raster)
library(hrbrthemes)

source("constants.R")

# KIV: Planning to get rid of this and load necessary codes at the top
source("1_initial_exploration.R")
source("2_hdb_exploration.R")


# Load Aedes Region Dataset
#===============================================================

aedes_regions <- readOGR("data/high_aedes_population_regions")
sf_aedes_regions <- st_as_sf(aedes_regions)
sf_aedes_regions <- st_transform(sf_aedes_regions, crs=3414)
head(sf_aedes_regions)


# Quick Plot to Visualise Locations of Aedes Hotspot and Skyrise Greenery
tmap_mode("plot")
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

# Compute Distance between Nearest Hotspot and All Buildings [This code will take some time (5-10 min) to complete]
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
  ggplot( aes(x=distance_from_nearest_hotspot, y=greenery, fill=greenery)) +
  geom_boxplot(outlier.shape = 2, outlier.size = 1) +
  scale_fill_manual(values=c("#e67c67", "#b8ffbc")) +
  xlab("Distance from Aedes Hotspot (m)") +
  ylab("Skyrise Greenery")

# Density Plot Visualisation
sf_combinded_skyrise_greenery_and_hdb %>%
  ggplot( aes(x=distance_from_nearest_hotspot, group=greenery, fill=greenery)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("#e67c67", "#b8ffbc")) +
  xlab("Distance from Aedes Hotspot (m)") +
  ylab("Density")


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

# Density - KDE
aedes_dens <- smooth_map(sf_aedes_regions_centroid, bandwidth = choose_bw(sf_aedes_regions_centroid$geometry))
tmap_mode('view')
tm_shape(aedes_dens$raster) + tm_raster() +
  tm_shape(sf_planning_area) +tm_borders(alpha=0.5) +
  tm_shape(sf_combinded_skyrise_greenery_and_hdb %>% filter(greenery == TRUE)) + tm_symbols(col="darkgreen", alpha=0.5, size = 0.1)


# Distance - K-Function
kf_aedes_hotspot <- Kest(as.ppp(sf_aedes_regions_centroid), correction = 'border')
plot(kf_aedes_hotspot, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf_aedes_hotspot.env <- envelope(as.ppp(sf_aedes_regions_centroid),Kest,correction="border")
plot(kf_aedes_hotspot.env)

