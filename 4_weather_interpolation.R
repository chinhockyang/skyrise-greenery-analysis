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
tmap_mode("plot")
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
tmap_mode("plot")
tm_shape(r_temp.m) + 
  tm_raster(n=10,palette = "-RdBu", auto.palette.mapping = FALSE,
            title="Predicted Temperature \n(in deg C)") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)


# Make Predicttion (based on Subzone)
sf_idw_temp_pred <- st_as_sf(Temp.idw)
subzone_predicted_temp <- st_join(sf_subzone, sf_idw_temp_pred, join=st_contains) %>% 
                                    group_by(subzone) %>% summarise(mean_temp = mean(var1.pred))
sf_skyrise_hdb <- st_join(sf_skyrise_hdb, subzone_predicted_temp, st_within)


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
tmap_mode("plot")
tm_shape(r_rainfall.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted Annual Rainfall") + 
  tm_shape(planning_area) + tm_borders(alpha = 0.6) +
  tm_legend(legend.outside=TRUE)


# Predict
sf_idw_rainfall_pred <- st_as_sf(Rainfall.idw)
subzone_predicted_rainfall <- st_join(sf_subzone, sf_idw_rainfall_pred, join=st_contains) %>% 
                                    group_by(subzone) %>% summarise(rainfall = mean(var1.pred))

sf_skyrise_hdb <- st_join(sf_skyrise_hdb, subzone_predicted_rainfall, st_within)


# -------------- Compare Temperature and Rainfall against Other Attributes  --------------

cor(sf_skyrise_hdb$mean_temp, sf_skyrise_hdb$rainfall)
cor(sf_skyrise_hdb$near_aedes_region, sf_skyrise_hdb$rainfall)
cor(sf_skyrise_hdb$near_aedes_region, sf_skyrise_hdb$mean_temp)

# TO CONTINUE TO ADD ANALYSIS


