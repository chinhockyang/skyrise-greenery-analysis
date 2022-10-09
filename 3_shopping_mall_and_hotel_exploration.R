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

# -------------- 1. Load Shopping Mall Dataset  --------------

# Derived from Web Scraping Google Maps
df_shopping_mall <- read.csv("data/shopping_mall_listing.csv", colClasses=c("postal_code"="character"))

# Postal Code Column get read as int, and the leading char "0" gets removed
df_shopping_mall$postal_code <- apply(df_shopping_mall, MARGIN=1, FUN = function(x) {
  if (nchar(x["postal_code"]) == 5) {
    return(paste("0", x["postal_code"], sep=""))
  }
  return(x["postal_code"])
})

sf_shopping_mall <- st_as_sf(df_shopping_mall, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_shopping_mall <- st_transform(sf_shopping_mall, crs= 3414)


# -------------- 2. Identify Skyrise Greenery Shopping Mall  --------------

# Extract Retail Skyrise Greenery
sf_skyrise_greenery_mall <- sf_skyrise_greenery %>% filter(type == "Retail") %>% select(postal_code = post_code, name, geometry)
sf_skyrise_greenery_mall$greenery <- TRUE

# Merge based on Postal Code
# (an alternative approach might be to add a smaller buffer to each point of 1 data, and use st_within to join)
sf_all_shopping_malls <- dplyr::full_join(df_shopping_mall %>% select(mall_names, postal_code, total_reviews, rating, latitude, longitude), sf_skyrise_greenery_mall)

sf_shopping_mall_w_skyrise_greenery_info <- rbind(

    # Shopping Malls from Scraped Dataset, but not in Skyrise Greenery Dataset
  dplyr::left_join(
    sf_all_shopping_malls 
        %>% filter(st_is_empty(geometry)) 
        %>% replace_na(list(greenery = FALSE)) 
        %>% select(mall_names, postal_code, greenery),
    sf_shopping_mall %>% select(mall_names, postal_code, total_reviews, rating)
  ),

  # Shopping Malls Exist in both Scraped Dataset and Skyrise Greenery Dataset
  sf_all_shopping_malls %>% filter(!st_is_empty(geometry) & !is.na(mall_names)) %>% select(mall_names = name, postal_code, geometry, greenery, total_reviews, rating),

    # Shopping Malls not in Scraped Dataset, but in Skyrise Greenery Dataset
    # Perform a Merge again, this time using the mall name column
    # Those still unmatched will be discarded (either mall name changed, or closed)
  merge(sf_all_shopping_malls %>% filter(!st_is_empty(geometry) & is.na(mall_names)) %>% select(mall_names = name, postal_code, geometry, greenery),
        df_shopping_mall %>% select(mall_names, total_reviews, rating))
)

# Convert from df to sf
sf_shopping_mall_w_skyrise_greenery_info <- st_as_sf(sf_shopping_mall_w_skyrise_greenery_info, crs=3414)

# -------------- 3. Skyrise Greenery vs Non-Skyrise Greenery Shopping Mall  --------------

sf_shopping_mall_w_skyrise_greenery_info %>% group_by(greenery) %>% count()

# -------------- 4. Location of Skyrise vs Non-Skyrise Shopping Mall  --------------

tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
tm_style(global.style) +
  tm_shape(sf_shopping_mall_w_skyrise_greenery_info) + 
  tm_layout(
    title = "Shopping Malls with and withouht Skyrise Greenery",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_dots(col = "greenery", id="mall_names", palette="Set1", size=0.5) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# -------------- 5. Total Reviews and Rating of Shopping Mall Exploration  --------------

# Get Rid of Anomalies
quantile(sf_shopping_mall_w_skyrise_greenery_info$total_reviews)
sf_shopping_mall_w_skyrise_greenery_info <- sf_shopping_mall_w_skyrise_greenery_info %>% filter(total_reviews > 100)

# Dot Map
tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
tm_style(global.style) +
  tm_shape(sf_shopping_mall_w_skyrise_greenery_info %>% filter(greenery == FALSE)) + 
  tm_layout(
    title = "Non-Skyrise Greenery Shopping Malls",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_bubbles("total_reviews", col = "rating", id="mall_names", palette="-RdYlBu") +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)

tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
  tm_style(global.style) +
  tm_shape(sf_shopping_mall_w_skyrise_greenery_info %>% filter(greenery == TRUE)) + 
  tm_layout(
    title = "Skyrise Greenery Shopping Malls",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_bubbles("total_reviews", col = "rating", id="mall_names", palette="-RdYlBu") +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# See only Total Review (Compare Skyrise Greenery vs Non-Skyrise Greenery in the same plot)
tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
tm_style(global.style) +
  tm_shape(sf_shopping_mall_w_skyrise_greenery_info) + 
  tm_layout(
    title = "Shopping Malls",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_bubbles("total_reviews", col = "greenery", id="mall_names", palette="-RdYlBu") +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)



# -------------- 6. Load Hotels Dataset  --------------

# Derived from Hotel Listing Board
df_hotel <- read.csv("data/hotel_listing.csv", colClasses=c("postal_code"="character"))
df_hotel$postal_code <- apply(df_hotel, MARGIN=1, FUN = function(x) {
  if (nchar(x["postal_code"]) == 5) {
    return(paste("0", x["postal_code"], sep=""))
  }
  return(x["postal_code"])
})

sf_hotel <- st_as_sf(df_hotel, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hotel <- st_transform(sf_hotel, crs= 3414)

# -------------- 7. Identify Skyrise Greenery Hotel --------------

sf_skyrise_hotel <- sf_skyrise_greenery %>% filter(type == 'Hotel') %>% rename(postal_code = post_code)
sf_skyrise_hotel$greenery <- TRUE

sf_all_hotels <- dplyr::full_join(df_hotel, sf_skyrise_hotel)

sf_all_hotels_w_skyrise_greenery_info <- rbind(

# Hotels from External Dataset, but not in Skyrise Greenery Dataset
  dplyr::left_join(
    sf_all_hotels 
    %>% filter(st_is_empty(geometry)) 
    %>% replace_na(list(greenery = FALSE)) 
    %>% select(hotel_names, postal_code, greenery),
    sf_hotel %>% select(hotel_names, postal_code)
  ),

  # Hotels in both datasets
  sf_all_hotels %>% filter(!st_is_empty(geometry) & !is.na(hotel_names)) %>% select(hotel_names, postal_code, greenery, geometry)
)

# Convert from df to sf
sf_all_hotels_w_skyrise_greenery_info <- st_as_sf(sf_all_hotels_w_skyrise_greenery_info, crs=3414)


# -------------- 8. Location of Skyrise vs Non-Skyrise Hotels  --------------

tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
tm_style(global.style) +
  tm_shape(sf_all_hotels_w_skyrise_greenery_info) + 
  tm_layout(
    title = "Hotels with and withouht Skyrise Greenery",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_dots(col = "greenery", id="hotel_names", palette="Set1", size=0.2) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# -------------- 9. Location of Skyrise Hotels vs Skyrise Shopping Malls  --------------

# Can actually use the original skyrise greenery dataset to compare
sf_shopping_mall_w_skyrise_greenery_info$type <- "Shopping Mall"
sf_all_hotels_w_skyrise_greenery_info$type <- "Hotel"
sf_skyrise_greenery_shopping_mall_hotel <- rbind(
    (sf_shopping_mall_w_skyrise_greenery_info %>% filter(greenery==TRUE) %>% rename(name=mall_names))[c("name", "type","geometry")],
    (sf_all_hotels_w_skyrise_greenery_info %>% filter(greenery==TRUE) %>% rename(name=hotel_names))[c("name", "type","geometry")]
)


tm_shape(sf_planning_area) + tm_fill(col="grey", alpha = 0.2) + tm_borders() +
tm_style(global.style) +
  tm_shape(sf_skyrise_greenery_shopping_mall_hotel) + 
  tm_layout(
    title = "Hotel and Shopping Mall with Skyrise Greenery",
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +
  tm_dots(col = "type", id="name", palette="Set1", size=0.2) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)
