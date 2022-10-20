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
hdb_info <- read.csv("data/addresses_full.csv")

# handle the 2 missing values
hdb_info <- hdb_info %>% drop_na(LATITUDE)

# Lowercase Address for Matching
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)

# # Convert to Sf
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

# -------------- Identify HDB that are Skyrise Greenery --------------
# lowercase address
sf_skyrise_greenery$address <- apply(sf_skyrise_greenery, MARGIN=1, FUN = function(x) {
  address = str_to_lower(x["address"])
  address = str_replace(address, "avenue", "ave")
  return(str_trim(str_replace(address, "singapore\\s*\\d*", "")))
})

# Merge by Postal Code or Address
# 150
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

lf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Lest,correction="border")
lf <- Lest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
plot(lf, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))


plot(lf.env)

gf <- Gest(as.ppp(sf_skyrise_hdb$geometry), main=NULL,correction="border")
pcf(as.ppp(sf_skyrise_hdb$geometry))
gf.env <- envelope(as.ppp(sf_skyrise_hdb$geometry),Gest,correction="border")
plot(gf.env)

# -------------- Density-Related Skyrise HDB Analysis --------------

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
  