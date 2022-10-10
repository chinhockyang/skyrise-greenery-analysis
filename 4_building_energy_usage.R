# -------------- Load Libraries and Variables --------------

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)

source("constants.R")
source("1_initial_exploration.R")

# -------------- 1. Load BCA Energy Dataset  --------------

df_bca <- read.csv("data/listing-of-building-energy-performance-data-2019.csv", colClasses=c("postal_code"="character"))

df_bca <- df_bca %>% filter((!is.na(latitude)) & (!is.na(longitude)))

# convert Char columns to numeric
df_bca$X2018energyuseintensity <- as.numeric(df_bca$X2018energyuseintensity)
df_bca$X2019energyuseintensity <- as.numeric(df_bca$X2019energyuseintensity)
df_bca$grossfloorarea <- as.numeric(gsub(",","", df_bca$grossfloorarea))

# Postal Code Column get read as int, and the leading char "0" gets removed
df_bca$postal_code <- apply(df_bca, MARGIN=1, FUN = function(x) {
  if (nchar(x["postal_code"]) == 5) {
    return(paste("0", x["postal_code"], sep=""))
  }
  return(x["postal_code"])
})

sf_bca <- st_as_sf(df_bca, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_bca <- st_transform(sf_bca, crs= 3414)

# -------------- 2. Map Buildings to Skyrise Greenery Dataset  --------------

# Level 1: Merge by Postal Code
df_greenery_bca_merge <- dplyr::left_join(sf_skyrise_greenery, df_bca %>% select(buildingname, buildingaddress,  postal_code), by=c("post_code"="postal_code")) %>% filter(!is.na(buildingname))
df_bca_is_greenery_1 <- df_greenery_bca_merge %>% filter(!is.na(buildingname)) %>% select(buildingname, buildingaddress, postal_code=post_code)

# Level 2: Merge using Buffer Containment

tmap_mode("view")
tm_shape(sf_bca) + tm_dots(col="red", size=0.05, id="buildingname") +
  tm_shape(st_buffer(sf_skyrise_greenery, 100)) + tm_fill(col="yellow", alpha = 0.1, id="name") + tm_borders(col="orange")


df_bca_is_greenery_2 <- st_join(sf_bca %>% filter(!(buildingname %in% df_bca_is_greenery_1$buildingname)), st_buffer(sf_skyrise_greenery, 50), st_within)  %>% filter(!is.na(post_code)) %>% select(buildingname, buildingaddress, postal_code=post_code)
df_bca_is_greenery <- rbind(df_bca_is_greenery_1, df_bca_is_greenery_2)

# Assign Skyrise and Non-Skyrise among the Buildings from BCA Dataset

sf_bca$greenery <- apply(sf_bca, MARGIN = 1, FUN = function(x) {
  if ((x["buildingname"] %in% df_bca_is_greenery$buildingname) && (x["buildingaddress"] %in% df_bca_is_greenery$buildingaddress)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# -------------- 3. EDA on difference between Greenery and Non-Greenery buildings  --------------

sf_bca %>% filter(greenery == TRUE) %>% group_by(greenmarkstatus) %>% count()
# No: 55
# Yes: 86

sf_bca %>% filter(greenery == FALSE) %>% group_by(greenmarkstatus) %>% count()
# No: 333
# Yes: 244

sf_bca %>% filter(greenery == TRUE) %>% group_by(greenmarkrating) %>% count()
sf_bca %>% filter(greenery == FALSE) %>% group_by(greenmarkrating) %>% count()

sf_bca %>% filter(greenery == TRUE) %>% 
  summarize(
    Mean_2018 = mean(X2018energyuseintensity, na.rm=TRUE),
    Mean_2019 = mean(X2019energyuseintensity, na.rm=TRUE),
    Mean_floorspace = mean(grossfloorarea, na.rm=TRUE)
  )

sf_bca %>% filter(greenery == FALSE) %>% 
  summarize(
    Mean_2018 = mean(X2018energyuseintensity, na.rm=TRUE),
    Mean_2019 = mean(X2019energyuseintensity, na.rm=TRUE),
    Mean_floorspace = mean(grossfloorarea, na.rm=TRUE)
  )
