
# Load Libraries and Variables
#===============================================================

library(stringr)
require(spatstat)
library(oldtmaptools)

source("constants.R")

# KIV: Planning to get rid of this and load necessary codes at the top
source("1_initial_exploration.R")


# Preparing External HDB dataset (Preprocessing)
#===============================================================
# Load Data
hdb_info <- read.csv("data/addresses_full.csv", colClasses=c("year_completed"="numeric", "max_floor_lvl"="numeric"))

# handle missing values
hdb_info <- hdb_info %>% drop_na(LATITUDE)

# Lowercase Address for Matching
hdb_info$ADDRESS <- str_to_lower(hdb_info$ADDRESS)

# Convert to SF
sf_hdb <- st_as_sf(hdb_info, coords = c("LONGITUDE", "LATITUDE"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sf_hdb <- st_transform(sf_hdb, crs= 3414)

# Building Type Assignment
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


# EDA on Full HDB Dataset 
#===============================================================

# Number of HDB in dataset
nrow(sf_hdb)
dplyr::count(sf_hdb, type, sort=TRUE)


# Density Plot
# [KIV]: Move to bottom for Point Pattern Analysis
choose_bw <- function(spdf) { 
  X <- st_coordinates(spdf) 
  sigma <- c(sd(X[,1]),sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6) 
  return(sigma/1000)
}

hdb_dens <- smooth_map(sf_hdb, bandwidth = choose_bw(sf_hdb$geometry))
tmap_mode('view')
  tm_shape(hdb_dens$raster) + tm_raster() +
  tm_shape(sf_subzone) +tm_borders()


# Pie Chart of Types of HDB
ggplot(dplyr::count(sf_hdb, type, sort=TRUE), aes(x="", y = n, fill = type)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + scale_fill_brewer(palette = "Pastel1")  

# Dot Plot of HDB (and their Types)
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_hdb, title="Type") + tm_symbols(col="type", size=0.4) +
  tm_style(global.style) +
  tm_layout(
    title = "HDB Buildings (and their types)", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# HDB Skyrise Greenery Identification
#===============================================================

# lowercase address on Skyrise Greenery dataset and standardising texts
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

# Create Boolean variable "greenery" to tell if building is skyrise greenery
sf_hdb$greenery <- apply(sf_hdb, MARGIN = 1, FUN = function(x) {
  if ((x["POSTAL_CODE"] %in% sf_skyrise_hdb$POSTAL_CODE) | (x["ADDRESS"] %in% sf_skyrise_hdb$ADDRESS)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# Combining All Unique Buildings from Both Datasets (use Address, Postal Code and greenery to filter and bind)
sf_skyrise_greenery$greenery <- TRUE
sf_combinded_skyrise_greenery_and_hdb <- rbind(
  sf_hdb %>% filter(greenery == FALSE ) %>% dplyr::select(ADDRESS, POSTAL_CODE, greenery),
  sf_skyrise_greenery %>% dplyr::select(address, post_code, greenery) %>% dplyr::rename(ADDRESS = address, POSTAL_CODE = post_code)
)
sf_combinded_skyrise_greenery_and_hdb <- st_join(sf_combinded_skyrise_greenery_and_hdb, sf_planning_area, join = st_within)
row.names(sf_combinded_skyrise_greenery_and_hdb) <- NULL


# Skyrise Greenery Exploratory Spatial Data Analysis
#===============================================================

# Locations of Skyrise HDB Buildings
tmap_mode("plot")
tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.8) + tm_borders(col="grey", alpha=1) +
  tm_shape(sf_skyrise_hdb, title="Type") + tm_symbols(col="type") +
  tm_style(global.style) +
  tm_layout(
    title = "HDB Skyrise Greenery in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Bar Chart of Types of HDB Skyrise Greenery
ggplot(dplyr::count(sf_skyrise_hdb, type, sort=TRUE), aes(x="", y = n, fill = type)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + scale_fill_brewer(palette = "Pastel1")


# Aggregate Data By Planning Area
# Look into mean Floor level, median Year Completed, number of buildings in each planning area
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

# Choropleth Visualisation - HDB across Planning Areas
tmap_mode('plot')
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("count", alpha=1, title="Number of Buildings") + tm_borders(alpha=0.9) +  
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Number of HDB across Planning Areas in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Year Completion
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("median_year_completed", alpha=1, title="Year Completed (Median)") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Year of Completion of HDB in Singapore", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# Choropleth Visualisation - Floor Level
tm_shape(sf_hdb_planning_area_info) + 
  tm_fill("mean_floor_lvl", alpha=1, title="Floor Level") + tm_borders(alpha=0.9) +
  tm_shape(sf_skyrise_hdb) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Floor Levels of HDB across Planning Areas", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 1, text.size = 0.8) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type=tm_compass.type, position=tm_compass.position, show.labels=tm_compass.show.labels, size=tm_compass.size)


# export out for use in hypo testing
# sf_hdb_planning_area_info <- sf_hdb_planning_area_info %>% dplyr::select(pln_area.x, geometry, mean_floor_lvl, median_year_completed, count) %>%
  # dplyr::rename(pln_area = pln_area.x, floor = mean_floor_lvl, year = median_year_completed)
# st_write(sf_hdb_planning_area_info, "hdb_info_accross_planning_area.shp", delete_layer = T)


# Point Pattern Analysis
#===============================================================

# -------------- Distance Related Skyrise HDB Analysis --------------

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

# KDE
skyrise_hdb_dens <- smooth_map(sf_skyrise_hdb, bandwidth = choose_bw(sf_skyrise_hdb$geometry))

tmap_mode('plot')
  tm_shape(skyrise_hdb_dens$raster) + tm_raster() +
  tm_shape(sf_subzone) +tm_borders()