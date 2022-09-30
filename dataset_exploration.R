
# -------------- Load Libraries --------------

# Packages from Lecture Practical Sheets
library(rgdal)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)


# Geojsonsf -> a package to read geojson files
geojsonsf_flag <- is.element("geojsonsf", installed.packages())
if (geojsonsf_flag==FALSE) {
  installed.packages("geojsonsf")
} else {
  library(geojsonsf)
}

# -------------- OneMap Planning Area Dataset  --------------
sg_planning_area <- readOGR("data/onemap_planning_area")
sf_sg_planning_area <- st_make_valid(st_as_sf(sg_planning_area))

tmap_mode("plot")
tm_shape(sf_sg_planning_area) +
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


# -------------- Skyrise Greenery --------------
skyrise_greenery <- readOGR("data/skyrise_greenery")
sf_skyrise_greenery <- st_as_sf(skyrise_greenery)
head(sf_skyrise_greenery)

tmap_mode("plot")
tm_shape(sf_sg_planning_area) + tm_fill(col="yellow", alpha=0.3) +
  tm_borders(alpha=0.5) +
  tm_style("natural") +
  tm_layout(
    title = "Skyrise Greenery in Singapore", 
    title.size = 2,
    title.fontface = "bold",
    title.fontfamily = "Palatino",
    title.position=c("left", "top"), 
    inner.margins = c(-0.02,0.03,0.1,0.03)
  ) +
  tm_shape(sf_skyrise_greenery) + tm_dots(size=0.1, col="darkblue") +
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
