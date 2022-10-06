# ------------------- Grouping weather data by quarters -----------------------

library(rgdal)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

weather_stations <- read.csv('../daily_temperature_2018_2020.csv')

weather_stations_map <- readOGR("../weather_stations")
sf_weather_stations <- st_as_sf(weather_stations_map)
head(sf_weather_stations)

# Retrieve relevant columns for both temp & rain
temp_data <- data.frame(weather_stations$station, weather_stations$year, weather_stations$month, 
                        weather_stations$minimum_temperature_.c., weather_stations$maximum_temperature_.c.)
temp_data$mean_temp <- (temp_data$weather_stations.minimum_temperature_.c. + temp_data$weather_stations.maximum_temperature_.c.)/2


summary(temp_data)

# Group by stations, then quarters
temp_data$seasons <- NA
temp_data$seasons[temp_data$weather_stations.month==1 | temp_data$weather_stations.month==2 | temp_data$weather_stations.month==3] <- "Q1" 
temp_data$seasons[temp_data$weather_stations.month==4 | temp_data$weather_stations.month==5 | temp_data$weather_stations.month==6] <- "Q2" 
temp_data$seasons[temp_data$weather_stations.month==7 | temp_data$weather_stations.month==8 | temp_data$weather_stations.month==9] <- "Q3" 
temp_data$seasons[temp_data$weather_stations.month==10 | temp_data$weather_stations.month==11 | temp_data$weather_stations.month==12] <- "Q4" 

temp_data <- temp_data %>% rename(station = weather_stations.station,
                                  year=weather_stations.year, month=weather_stations.month)
# remove rows with NA temp values
temp_data <- temp_data[!is.na(temp_data$mean_temp), ]
# mean temp in each season for each weather stations
temp_grouped <- temp_data %>% group_by(station, seasons, year) %>% summarise_at(vars(mean_temp), list(mean))

# add coordinates
temp_grouped <- merge(temp_grouped, sf_weather_stations, all.x=T )

write.csv(temp_grouped,"../grouped_temp_data.csv", row.names = FALSE)


## Rainfall
rain_data <- data.frame(weather_stations$station, weather_stations$year, weather_stations$month, 
                        weather_stations$daily_rainfall_total_.mm.)

summary(rain_data)

# Group by stations, then quarters
rain_data$seasons <- NA
rain_data$seasons[rain_data$weather_stations.month==1 | rain_data$weather_stations.month==2 | rain_data$weather_stations.month==3] <- "Q1" 
rain_data$seasons[rain_data$weather_stations.month==4 | rain_data$weather_stations.month==5 | rain_data$weather_stations.month==6] <- "Q2" 
rain_data$seasons[rain_data$weather_stations.month==7 | rain_data$weather_stations.month==8 | rain_data$weather_stations.month==9] <- "Q3" 
rain_data$seasons[rain_data$weather_stations.month==10 | rain_data$weather_stations.month==11 | rain_data$weather_stations.month==12] <- "Q4" 

rain_data <- rain_data %>% rename(station = weather_stations.station,
                                  year=weather_stations.year, month=weather_stations.month,
                                  rainfall=weather_stations.daily_rainfall_total_.mm.)
# remove rows with NA temp values
rain_data <- rain_data[!is.na(rain_data$rainfall), ]
# sum of rainfall in each season for each weather stations every year
rain_grouped <- rain_data %>% group_by(station, seasons, year) %>% summarise_at(vars(rainfall), list(sum))

rain_grouped <- merge(rain_grouped, sf_weather_stations, all.x=T )
write.csv(rain_grouped,"../grouped_rainfall_data.csv", row.names = FALSE)
