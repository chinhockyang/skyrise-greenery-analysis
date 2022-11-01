# -------------- Load Libraries and Variables --------------

library(gstat)
require(deldir)
library(raster)

source("1_initial_exploration.R")
source("2_hdb_exploration.R")
source("3_aedes_region_analysis.R")

# -------------- Load Weather, Temperature and Rainfall Dataset --------------

weather_stations <- readOGR("data/weather_stations")
sf_weather_stations <- st_as_sf(weather_stations)
sf_weather_stations <- st_transform(sf_weather_stations, crs=3414)

df_rainfall_data <- read.csv("data/groupbystation_rainfall_2019.csv")
df_temperature_data <- read.csv("data/groupbystation_temp_2019.csv")


# Convert to SF
sf_rainfall_data <- merge(sf_weather_stations, df_rainfall_data)
sf_temperature_data <- merge(sf_weather_stations, df_temperature_data)


# -------------- EDA (Temperature) --------------

# Formatting
sf_temperature_data$mean_temp_2dp <- sprintf("%.2f", sf_temperature_data$mean_temp)

# Plot
# ----- Blue Means Lower Temperature, Red Means Higher Temperature
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="grey", alpha=0.4) + tm_borders(alpha=0.4) +
  tm_shape(sf_temperature_data) +
  tm_symbols(col="mean_temp", palette = "-RdBu") +
  tm_shape(sf_temperature_data) +
  tm_dots(col="mean_temp", palette = "-RdBu",
          title="Mean Temperature (2019) \n(deg C)", size=0.7) +
  tm_text("mean_temp_2dp", just="left", xmod=.5, size = 1) +
  tm_legend(legend.outside=TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# -------------- EDA (Rainfall) --------------

# Formatting
sf_rainfall_data$rainfall_2dp <- sprintf("%.2f", sf_rainfall_data$rainfall)

# Plot
# ----- Blue Means More Rainfall, Red Means Less Rainfall
tm_shape(sf_planning_area) + tm_fill(col="grey", alpha=0.4) + tm_borders(alpha=0.4) +
  tm_shape(sf_rainfall_data) +
  tm_symbols(col="rainfall", palette = "RdBu") +
  tm_shape(sf_rainfall_data) +
  tm_dots(col="rainfall", palette = "RdBu",
          title="Total Rainfall in 2019 (mm)", size=0.7) +
  tm_text("rainfall_2dp", just="left", xmod=.5, size = 1) +
  tm_legend(legend.outside=TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

# -------------- Preparing SP data for Interpolation --------------

planning_area <- as(sf_planning_area, "Spatial")
weather_stations <- spTransform(weather_stations, proj4string(planning_area))

sp_rainfall <- as(sf_rainfall_data, "Spatial")
sp_temperature <- as(sf_temperature_data, "Spatial")
sp_rainfall@bbox <- planning_area@bbox
sp_temperature@bbox <- planning_area@bbox
crs(sp_rainfall) <- crs(planning_area)
crs(sp_temperature) <- crs(planning_area)


# -------------- IDW Interpolate Temperature --------------

# Further Processing Data for Interpolation
grd_temp              <- data.frame(spsample(sp_temperature, "regular", n=50000))
names(grd_temp)       <- c("X", "Y")
coordinates(grd_temp) <- c("X", "Y")
gridded(grd_temp)     <- TRUE
fullgrid(grd_temp)    <- TRUE

proj4string(grd_temp) <- proj4string(sp_temperature)
crs(grd_temp) <- crs(sp_temperature)

# Hyper-parameter Tuning of IDP (n order)
temp_hyper_param_tuning <- data.frame()
for (n in 1:5) {
  Temp.idw.out <- vector(length= nrow(sp_temperature))
  for (i in 1:nrow(sp_temperature)) {
    Temp.idw.out[i] <- gstat::idw(mean_temp ~ 1, sp_temperature[-i,], sp_temperature[i,], idp=n)$var1.pred
  }
  rmse = sqrt( sum((Temp.idw.out - sp_temperature$mean_temp)^2) / nrow(sp_temperature))
  temp_hyper_param_tuning <- rbind(temp_hyper_param_tuning, data.frame(n=c(n), rmse=c(rmse)))
}

plot(temp_hyper_param_tuning)
### RESULT: n_order 2 have the lowest rmse

# Interpolation
Temp.idw <- gstat::idw(mean_temp ~ 1, sp_temperature, newdata=grd_temp, idp=2.0)
r_temp       <- raster(Temp.idw)
r_temp.m     <- mask(r_temp, planning_area)


# Plot
tmap_mode("plot")
tm_shape(r_temp.m) + 
  tm_raster(n=10,palette = "-RdBu", auto.palette.mapping = FALSE,
            title="Predicted Temperature \n(in deg C)") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)


# Make Predicttion (based on Subzone)
sf_idw_temp_pred <- st_as_sf(Temp.idw)
planning_area_predicted_temp <- st_join(sf_planning_area, sf_idw_temp_pred, join=st_contains) %>% 
                                    group_by(pln_area_n) %>% summarise(mean_temp = mean(var1.pred))


planning_area_predicted_temp

# -------------- IDW Interpolate Rainfall --------------

# Further Processing Data for Interpolation
grd_rainfall              <- data.frame(spsample(sp_rainfall, "regular", n=50000))
names(grd_rainfall)       <- c("X", "Y")
coordinates(grd_rainfall) <- c("X", "Y")
gridded(grd_rainfall)     <- TRUE
fullgrid(grd_rainfall)    <- TRUE

proj4string(grd_rainfall) <- proj4string(sp_rainfall)
crs(grd_rainfall) <- crs(sp_rainfall)

# Hyper-parameter Tuning of IDP (n order)
rainfall_hyper_param_tuning <- data.frame()
for (n in 1:5) {
  Rainfall.idw.out <- vector(length= nrow(sp_rainfall))
  for (i in 1:nrow(sp_rainfall)) {
    Rainfall.idw.out[i] <- gstat::idw(rainfall ~ 1, sp_rainfall[-i,], sp_rainfall[i,], idp=n)$var1.pred
  }
  rmse = sqrt( sum((Rainfall.idw.out - sp_rainfall$rainfall)^2) / nrow(sp_rainfall))
  rainfall_hyper_param_tuning <- rbind(rainfall_hyper_param_tuning, data.frame(n=c(n), rmse=c(rmse)))
}

plot(rainfall_hyper_param_tuning)
rainfall_hyper_param_tuning
### RESULT: n_order 3 have the lowest rmse

# Interpolation
Rainfall.idw <- gstat::idw(rainfall ~ 1, sp_rainfall, newdata=grd_rainfall, idp=3.0)
r_rainfall       <- raster(Rainfall.idw)
r_rainfall.m     <- mask(r_rainfall, planning_area)

# Plot
tmap_mode("plot")
tm_shape(r_rainfall.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted Annual Rainfall") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)

# Predict
sf_idw_rainfall_pred <- st_as_sf(Rainfall.idw)
planning_area_predicted_rainfall <- st_join(sf_planning_area, sf_idw_rainfall_pred, join=st_contains) %>% 
                                    group_by(pln_area_n) %>% summarise(rainfall = mean(var1.pred))



# -------------- Export Predicted Dataset --------------

st_write(planning_area_predicted_temp, "interpolated_temperature.shp", delete_layer = T)
st_write(planning_area_predicted_rainfall, "interpolated_rainfall.shp", delete_layer = T)