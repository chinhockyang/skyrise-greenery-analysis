# ------------------- Grouping weather data by quarters -----------------------

library(rgdal)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

weather_stations <- read.csv('data/daily_temperature_2018_2020.csv')

weather_stations_map <- readOGR("data/weather_stations")
sf_weather_stations <- st_as_sf(weather_stations_map)
head(sf_weather_stations)

# Retrieve relevant columns for both temp & rain
temp_data <- data.frame(weather_stations$station, weather_stations$year, weather_stations$month, 
                        weather_stations$minimum_temperature_.c., weather_stations$maximum_temperature_.c.)
temp_data$mean_temp <- (temp_data$weather_stations.minimum_temperature_.c. + temp_data$weather_stations.maximum_temperature_.c.)/2
temp_data <- temp_data %>% rename(station = weather_stations.station,
                                  year=weather_stations.year, month=weather_stations.month)

summary(temp_data)

# Group by stations, then quarters
temp_data$seasons <- NA
temp_data$seasons[temp_data$month==1 | temp_data$month==2 | temp_data$month==3] <- "Q1" 
temp_data$seasons[temp_data$month==4 | temp_data$month==5 | temp_data$month==6] <- "Q2" 
temp_data$seasons[temp_data$month==7 | temp_data$month==8 | temp_data$month==9] <- "Q3" 
temp_data$seasons[temp_data$month==10 | temp_data$month==11 | temp_data$month==12] <- "Q4" 

# remove rows with NA temp values
temp_data <- temp_data[!is.na(temp_data$mean_temp), ]

# retrieve 2019 temp data
temp_2019 <- temp_data %>% filter(year==2019)

# version1: mean temp in each season for each weather stations
temp_grouped_seasons <- temp_2019 %>% group_by(station, seasons) %>% summarise_at(vars(mean_temp), list(mean))
# version2: no seasons grouping to get 1 temp for each station
temp_grouped <- temp_2019 %>% group_by(station) %>% summarise_at(vars(mean_temp), list(mean))

# add coordinates
temp_grouped <- merge(temp_grouped, sf_weather_stations, all.x=T )

write.csv(temp_grouped,"data/grouped_temp_2019.csv", row.names = FALSE)


## Rainfall
rain_data <- data.frame(weather_stations$station, weather_stations$year, weather_stations$month, 
                        weather_stations$daily_rainfall_total_.mm.)
rain_data <- rain_data %>% rename(station = weather_stations.station,
                                  year=weather_stations.year, month=weather_stations.month,
                                  rainfall=weather_stations.daily_rainfall_total_.mm.)
summary(rain_data)

# Group by stations, then quarters
rain_data$seasons <- NA
rain_data$seasons[rain_data$month==1 | rain_data$month==2 | rain_data$month==3] <- "Q1" 
rain_data$seasons[rain_data$month==4 | rain_data$month==5 | rain_data$month==6] <- "Q2" 
rain_data$seasons[rain_data$month==7 | rain_data$month==8 | rain_data$month==9] <- "Q3" 
rain_data$seasons[rain_data$month==10 | rain_data$month==11 | rain_data$month==12] <- "Q4" 


# remove rows with NA temp values
rain_data <- rain_data[!is.na(rain_data$rainfall), ]

# retrieve 2019 temp data
rain_2019 <- rain_data %>% filter(year==2019)

# version1: sum of rainfall in each season for each weather stations every year
rain_grouped_seasons <- rain_2019 %>% group_by(station, seasons) %>% summarise_at(vars(rainfall), list(sum))
# version2: yearly rainfall sum for each station
rain_grouped <- rain_2019 %>% group_by(station) %>% summarise_at(vars(rainfall), list(sum))

# add coordinates
rain_grouped <- merge(rain_grouped, sf_weather_stations, all.x=T )

write.csv(rain_grouped,"data/grouped_rainfall_2019.csv", row.names = FALSE)
