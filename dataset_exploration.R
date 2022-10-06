
# -------------- Load Libraries --------------

# Packages from Lecture Practical Sheets
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)

library(tmaptools)
#tmaptools::palette_explorer()

# -------------- OneMap Planning Area Dataset  --------------

# Load Dataset
sg_planning_area <- readOGR("data/onemap_planning_area")
sf_sg_planning_area <- st_make_valid(st_as_sf(sg_planning_area))
# reset_index() equivalence in Pandas
row.names(sf_sg_planning_area) <- NULL

# Convert to EPSG:3414
st_transform(sf_sg_planning_area, crs=3414)

tmap_mode("plot")
tm_shape(sf_sg_planning_area) +
  tm_fill(col="yellow", alpha=0.3) +
  tm_borders(alpha=0.8) +
  tm_style("natural") +
  tm_layout(
    title = "Singapore Planning Area", 
    title.size = 1.5, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.05,0.05,0.1,0.03)
  ) +
tm_scale_bar(position = c("right", "bottom")) +
tm_compass(type="rose", position=c("right", "top"), show.labels = 2, size = 2)


# -------------- Skyrise Greenery --------------
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_as_sf(skyrise_greenery)
# Convert to EPSG:3414
st_transform(sf_skyrise_greenery, crs=3414)

####### Vector: Dot Map (EDA)
tmap_mode("plot")
tm_shape(sf_sg_planning_area) + tm_fill(col="yellow", alpha=0.3) +
  tm_borders(alpha=0.5) +
  tm_style("natural") +
  tm_layout(
    title = "Dot Map of Skyrise Buildings in Singapore", 
    title.size = 1.5, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.05,0.05,0.1,0.03)
  ) +
  tm_shape(sf_skyrise_greenery) + tm_dots(size=0.1, col="type", alpha = 1, palette="Accent") +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type="rose", position=c("right", "top"), show.labels = 3, size = 2)

####### Vector: Containment of Skyrise Building within Planning Area
# Output --> SF with the Number of Buildings for each Type of Buildings for each Planning Area, geometry are Multipoints/Points of the buildings
# |---- pln_area_n ----| ---- type ----|---- n ----|---- geometry ----|
sf_skyrise_grpby_type_planning_area <- st_join(sf_skyrise_greenery, 
                                               sf_sg_planning_area, 
                                               join = st_within, 
                                               left = TRUE) %>% 
                                      dplyr::select("name","type","pln_area_n")%>%
                                      group_by(pln_area_n, type) %>% 
                                      count()
                                  
sf_planning_area_w_skyrise_info <- st_join(sf_sg_planning_area %>% select(geometry), 
                                                sf_skyrise_grpby_type_planning_area, 
                                               join = st_contains)

####### Vector: Choropleth Map of Skyrise HDB across Planning Areas (EDA)
tmap_mode("plot")
tm_shape(sf_sg_planning_area) + tm_fill(col="white", alpha = 0.4) + tm_borders(col="grey", alpha=0.5) +
tm_shape(
  sf_planning_area_w_skyrise_info %>% filter(type %in% c("HDB"))) + 
  tm_polygons("n", alpha=1, title="Skyrise HDB", breaks=c(0,5,10,15,20)) +
  tm_borders(alpha=0.9) +
  tm_style("natural") +
  tm_layout(
    title = "Skyrise HDB across Planning Areas", 
    title.size = 1.5, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.05,0.05,0.1,0.03),
  ) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_compass(type="rose", position=c("right", "top"), show.labels = 3, size = 2)


####### Vector: Clipping of URA
urban_area <- readOGR("data/urban_design_area")
sf_urban_area <- st_make_valid(st_as_sf(urban_area))
st_transform(sf_urban_area, crs=3414)

# Match Planning Area Name to Urban Design Areas
# [TODO: Review Past Notes to see how to "kill" variable from global environment]
sf_urban_area <- st_join(sf_urban_area, sf_sg_planning_area, join = st_intersects)
sf_urban_area$pln_area_n <- sf_urban_area$pln_area_n %>% tidyr::replace_na(NA)
sf_urban_area_no_pln_area <- sf_urban_area %>% filter(is.na(pln_area_n))
sf_nearest_planning_area <- st_nearest_feature(sf_urban_area_no_pln_area, sf_sg_planning_area)
for (i in 1:length(sf_nearest_planning_area)) {
  sf_urban_area_no_pln_area[i,"pln_area_n"] <- sf_sg_planning_area[i,]$pln_area_n
}
sf_urban_area <- rbind(sf_urban_area %>% filter(!is.na(pln_area_n)), sf_urban_area_no_pln_area)
row.names(sf_urban_area) <- NULL

sf_cbd_area <- sf_urban_area %>% select(status, pln_area_n, geometry) %>% 
  filter(pln_area_n %in% c("MARINA EAST", "NEWTON", "DOWNTOWN CORE", 
                           "KALLANG", "MARINA SOUTH", "MUSUEM", "NEWTON", "ORCHARD", "NOVENA", "RIVER VALLEY", "ROCHOR"))

# CLIPPING
sf_skyrise_greenery_within_cbd <- st_intersection(sf_skyrise_greenery, sf_cbd_area %>% st_union())
sf_skyrise_greenery_within_cbd %>% group_by(type) %>% count()

tmap_mode("plot")
tm_shape(sf_cbd_area) + tm_fill(col="yellow") +
  tm_borders(alpha=0.5) +
  tm_style("natural") +
  tm_layout(
    title = "Dot Map of Skyrise Buildings in Singapore", 
    title.size = 1.5, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.05,0.05,0.1,0.03)
  ) +
  tm_shape(sf_skyrise_greenery_within_cbd) + tm_dots(size=0.1, col="type", alpha = 1, palette="Accent") +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type="rose", position=c("right", "top"), show.labels = 3, size = 2)


# -------------- Weather Stations  --------------

weather_stations <- readOGR("data/weather_stations")
sf_weather_stations <- st_as_sf(weather_stations)
head(sf_weather_stations)

tmap_mode("plot")
tm_shape(sf_sg_planning_area) + tm_fill(col="yellow", alpha=0.3) +
  tm_borders(alpha=0.5) +
  tm_style("natural") +
  tm_layout(
    title = "Weather Stations in Singapore", 
    title.size = 2,
    title.fontface = "bold",
    title.fontfamily = "Palatino",
    title.position=c("left", "top"), 
    inner.margins = c(-0.02,0.03,0.1,0.03)
  ) +
  tm_shape(weather_stations) + tm_dots(size=0.1, col="darkblue") +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type="rose", position=c("right", "top"), show.labels = 3, size = 2)


# -------------- Areas with High Aedes Population  --------------
aedes_regions <- readOGR("data/high_aedes_population_regions")
sf_aedes_regions <- st_as_sf(aedes_regions)
head(sf_aedes_regions)

tmap_mode("plot")
tm_shape(sf_aedes_regions) +
  tm_fill(col="yellow", alpha=0.3) +
  tm_style("natural") +
  tm_borders(alpha=0.8) +
  tm_layout(
    title = "Singapore Planning Area", 
    title.size = 2, 
    title.position=c("left", "top"), 
    title.fontface= "bold", 
    title.fontfamily= "Palatino", 
    inner.margins = c(-0.02,0.03,0.1,0.03)
  ) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type="rose", position=c("right", "top"), show.labels = 2, size = 2)


# -------------- Daily Temperature (2018-2020)  --------------
df_daily_temp <- read.csv("data/daily_temperature_2018_2020.csv")
head(df_daily_temp)

df_daily_temp$avg_temperature <- apply(
  df_daily_temp %>% select(minimum_temperature_.c., maximum_temperature_.c., mean_temperature_.c.),
  MARGIN = 1,
  FUN = function(x) {
    if (!is.na(x["mean_temperature_.c."])) {
      return(x["mean_temperature_.c."]) 
    } else if ((!is.na(x["min_temperature_.c."])) && (!is.na(x["max_temperature_.c."]))) {
      return((x["min_temperature_.c."] + x["max_temperature_.c."]) / 2)
    } else {
      return(NA)
    }
  }
)

df_station_mean_daily_temp <- df_daily_temp %>% select(station, avg_temperature) %>%
                  group_by(station) %>% summarize(mean_avg_temperature = mean(avg_temperature, na.rm=TRUE)) %>%
                  filter(!is.na(mean_avg_temperature))

sf_weather_stations <- merge(sf_weather_stations, df_station_mean_daily_temp, on = "station")
head(sf_weather_stations)
