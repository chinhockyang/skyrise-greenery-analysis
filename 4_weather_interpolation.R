# -------------- Load Libraries and Variables --------------

library(rgdal)
library(sf)
library(dplyr)
library(gstat)
require(deldir)
library(tmap)
library(gstat)
library(raster)

source("1_initial_exploration.R")
source("2_hdb_exploration.R")

# -------------- Load Weather, Temperature and Rainfall Dataset --------------
weather_stations <- readOGR("data/weather_stations")
sf_weather_stations <- st_as_sf(weather_stations)
sf_weather_stations <- st_transform(sf_weather_stations, crs=3414)

df_rainfall_data <- read.csv("data/grouped_rainfall_data.csv")
df_temperature_data <- read.csv("data/grouped_temp_data.csv")

# -------------- Subset Rainfall/Temperature Data --------------

df_total_annual_rainfall <- df_rainfall_data %>% group_by(station, year) %>% summarise(rainfall = sum(rainfall, na.rm=TRUE))
df_mean_quarter_temp <- df_temperature_data %>% group_by(station, seasons) %>% summarise(mean_temp = mean(mean_temp, na.rm=TRUE))

# 2019
df_2019_rainfall <- df_total_annual_rainfall %>% filter(year == 2019)
# Q3
df_mean_q3_temp <- df_mean_quarter_temp %>% filter(seasons == "Q3")

# Used for analysis
sf_rainfall_data <- merge(sf_weather_stations, df_2019_rainfall)
sf_temperature_data <- merge(sf_weather_stations, df_mean_q3_temp)


# -------------- EDA (Temperature) --------------

sf_temperature_data$mean_temp_2dp <- sprintf("%.2f", sf_temperature_data$mean_temp)
tm_shape(sf_planning_area) + tm_polygons() +
  tm_shape(sf_temperature_data) +
  tm_dots(col="mean_temp", palette = "-RdBu",
          title="Mean Temperature (Q3) \n(deg C)", size=0.7) +
  tm_text("mean_temp_2dp", just="left", xmod=.5, size = 1) +
  tm_legend(legend.outside=TRUE)


# -------------- EDA (Rainfall) --------------

sf_rainfall_data$rainfall_2dp <- sprintf("%.2f", sf_rainfall_data$rainfall)
tm_shape(sf_planning_area) + tm_polygons() +
  tm_shape(sf_rainfall_data) +
  tm_dots(col="rainfall", palette = "RdBu",
          title="Total Rainfall in 2019", size=0.7) +
  tm_text("rainfall_2dp", just="left", xmod=.5, size = 1) +
  tm_legend(legend.outside=TRUE)


# -------------- Preparing SP data --------------

planning_area <- as(sf_planning_area, "Spatial")
weather_stations <- spTransform(weather_stations, proj4string(planning_area))

sp_rainfall <- as(sf_rainfall_data, "Spatial")
sp_temperature <- as(sf_temperature_data, "Spatial")
sp_rainfall@bbox <- planning_area@bbox
sp_temperature@bbox <- planning_area@bbox
crs(sp_rainfall) <- crs(planning_area)
crs(sp_temperature) <- crs(planning_area)


# -------------- IDW Interpolate Temperature --------------
grd_temp              <- data.frame(spsample(sp_temperature, "regular", n=50000))
names(grd_temp)       <- c("X", "Y")
coordinates(grd_temp) <- c("X", "Y")
gridded(grd_temp)     <- TRUE
fullgrid(grd_temp)    <- TRUE

proj4string(grd_temp) <- proj4string(sp_temperature)
crs(grd_temp) <- crs(sp_temperature)

Temp.idw <- gstat::idw(mean_temp ~ 1, sp_temperature, newdata=grd_temp, idp=3.0)
r_temp       <- raster(Temp.idw)
r_temp.m     <- mask(r_temp, planning_area)

# Plot
tm_shape(r_temp.m) + 
  tm_raster(n=10,palette = "-RdBu", auto.palette.mapping = FALSE,
            title="Predicted Temperature \n(in deg C)") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)


tmap_mode("plot")

# Predict
sf_idw_temp_pred <- st_as_sf(Temp.idw)
subzone_predicted_temp <- st_join(sf_subzone, sf_idw_temp_pred, join=st_contains) %>% 
                                    group_by(subzone) %>% summarise(mean_temp = mean(var1.pred))


sf_skyrise_greenery <- st_join(sf_skyrise_greenery, subzone_predicted_temp, st_within)

tm_shape(sf_skyrise_greenery) +
  tm_dots(col="mean_temp")

# -------------- IDW Interpolate Rainfall --------------
grd_rainfall              <- data.frame(spsample(sp_rainfall, "regular", n=50000))
names(grd_rainfall)       <- c("X", "Y")
coordinates(grd_rainfall) <- c("X", "Y")
gridded(grd_rainfall)     <- TRUE
fullgrid(grd_rainfall)    <- TRUE

proj4string(grd_rainfall) <- proj4string(sp_rainfall)
crs(grd_rainfall) <- crs(sp_rainfall)

Rainfall.idw <- gstat::idw(rainfall ~ 1, sp_rainfall, newdata=grd_rainfall, idp=3.0)
r_rainfall       <- raster(Rainfall.idw)
r_rainfall.m     <- mask(r_rainfall, planning_area)

# Plot
tm_shape(r_rainfall.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted Annual Rainfall") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)


tmap_mode("plot")

# Predict
sf_idw_rainfall_pred <- st_as_sf(Rainfall.idw)
subzone_predicted_rainfall <- st_join(sf_subzone, sf_idw_rainfall_pred, join=st_contains) %>% 
                                    group_by(subzone) %>% summarise(rainfall = mean(var1.pred))

sf_skyrise_greenery <- st_join(sf_skyrise_greenery, subzone_predicted_rainfall, st_within)


tm_shape(sf_skyrise_greenery) +
  tm_dots(col="rainfall")




# -------------- DRAFT --------------

# Nearest Neighbour Interpolation

voronoipolygons = function(layer) {
  layer <- as(layer, "Spatial")
  #crds = st_coordinates(layer$geometry)
  crds <- layer@coords
  print(crds[,1])
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, 
                                     proj4string= proj4string(as(sf_weather_stations, "Spatial")),
                                     data=data.frame(x=crds[,1],y=crds[,2], 
                                                         layer@data, row.names=sapply(slot(SP, 'polygons'),
                                                        function(x) slot(x, 'ID'))))
  return(voronoi)
}

test <- sf_rainfall_data %>% filter(year == 2019 & seasons == "Q4")
sf_rainfall.voro <- voronoipolygons(test)
sf_rainfall.voro <- st_as_sf(sf_rainfall.voro, crs=3414)
st_crs(sf_rainfall.voro) <- st_crs(sf_planning_area)

tm_shape(sf_rainfall.voro) +
  tm_fill(col='rainfall',style='fixed',breaks=c(100,200,300,400,500,600,700,800,900,1000), 
          alpha=0.6,title='Rainfall Density')+ tm_borders(alpha=0.05) +
  tm_shape(sf_planning_area) + tm_borders()

