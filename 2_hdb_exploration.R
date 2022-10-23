# -------------- Load Libraries and Variables --------------

library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(ggplot2)
library(tmaptools)
# tmaptools::palette_explorer()

library(stringr)
require(spatstat)

source("constants.R")
source("1_initial_exploration.R")

# -------------- Prepare HDB dataset --------------
hdb_info <- read.csv("data/addresses_full.csv", colClasses=c("year_completed"="numeric", "max_floor_lvl"="numeric"))

# handle missing values
hdb_info <- hdb_info %>% drop_na(LATITUDE)

# Lowercase Address for Matching
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)

# # Convert to Sf
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

sf_hdb$type <- apply(sf_hdb, MARGIN=1, FUN=function(x) {
  if(x["residential"] == "Y") {
    return("Residential")
  } else if (x["multistorey_carpark"] == "Y") {
    return("MSCP")
  } else if (x["commercial"] == "Y") {
    return("Commercial")
  } else if (x["market_hawker"] == "Y") {
    return("Market/Hawker")
  } else if (x["precint_pavilion"] == "Y") {
    return("Pavillion")
  } else {
    return("Others")
  }
})

# -------------- Identify HDB that are Skyrise Greenery --------------

# lowercase address
sf_skyrise_greenery$address <- apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  address = str_to_lower(x["address"])
  address = str_replace(address, "avenue", "ave")
  return(str_trim(str_replace(address, "singapore\\s*\\d*", "")))
})

# Merge by Postal Code or Address
sf_skyrise_hdb <- sf_hdb %>% filter(
    (POSTAL_CODE %in% sf_skyrise_greenery$post_code) |
    (ADDRESS %in% sf_skyrise_greenery$address)
)

# -------------- Distance Related Skyrise HDB Analysis --------------

qtm(sf_skyrise_hdb)

kf <- Kest(as.ppp(sf_skyrise_hdb$geometry), correction = 'border')
plot(kf, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE))
kf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Kest,correction="border")
plot(kf.env)

## Envelop
lf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Lest,correction="border")
lf <- Lest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
plot(lf.env)
plot(lf, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))


## Envelop
gf <- Gest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
gf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Gest,correction="border")
plot(gf.env)

# Pair Correlation Function g
g <- pcf(as.ppp(sf_skyrise_hdb$geometry))
plot(g)


# -------------- Density-Related Skyrise HDB Analysis --------------

# Quadrat
Q <- quadratcount(as.ppp(sf_skyrise_hdb), nx=6, ny=3)
plot(as.ppp(sf_skyrise_hdb), pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)

Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(as.ppp(sf_skyrise_hdb), pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

### KDE
library(oldtmaptools)

choose_bw <- function(spdf) { 
  X <- st_coordinates(spdf) 
  sigma <- c(sd(X[,1]),sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6) 
  return(sigma/1000)
}

breach_dens <- smooth_map(sf_skyrise_hdb, bandwidth = choose_bw(sf_skyrise_hdb$geometry))

tmap_mode('view')
  tm_shape(breach_dens$raster) + tm_raster() +
  tm_shape(sf_subzone) +tm_borders()
  

# -------------- Attributes Spatial Analysis --------------

sf_hdb %>% group_by(type) %>% summarise(count=n_distinct(POSTAL_CODE))

# Types of Skyrise Buildings
ggplot(sf_skyrise_hdb %>% group_by(type) %>% summarise(count=n_distinct(POSTAL_CODE)), 
  aes(x=type, y=count)) + geom_bar(stat="identity")


### HDB in General
sf_hdb_by_planning_area <- st_join(sf_hdb, sf_planning_area, join = st_within)
sf_hdb_group_by_planning_area <- sf_hdb_by_planning_area %>% 
                              filter(residential == "Y") %>% 
                              group_by(pln_area) %>%
                              summarise(
                                mean_floor_lvl = mean(max_floor_lvl),
                                median_year_completed = median(year_completed, na.rm=FALSE),
                                count = n_distinct(POSTAL_CODE)
                              )

sf_hdb_planning_area_info <- st_join(sf_planning_area,
                                    sf_hdb_group_by_planning_area,
                                    join = st_contains) %>% replace_na(list(count = 0))


# Visualise HDB across Planning Areas (with Skyrise HDB)
tmap_mode('view')
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("count", alpha=1, title="Number of HDB") + tm_borders(alpha=0.9) +  
  tm_shape(sf_skyrise_hdb) + tm_dots(col="type") +
  tm_style(global.style) +
  tm_layout(
    title = "HDBs across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Year Completion of HDB across Planning Areas (With Skyrise HDB)
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("median_year_completed", alpha=1, title="Year Completed (Median)") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_dots(col="yellow") +
  tm_style(global.style) +
  tm_layout(
    title = "Year of Completion of HDB in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Year Completion of HDB across Planning Areas (With Skyrise HDB)
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("mean_floor_lvl", alpha=1, title="Number of HDB") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="multistorey_carpark", size=0.6, palette="RdYlGn", border.alpha=1) +
  tm_style(global.style) +
  tm_layout(
    title = "Floor Levels of HDBs in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("left", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)