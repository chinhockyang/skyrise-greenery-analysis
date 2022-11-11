# Complete Spatial Randomness Study (Skyrise Greenery)

# Load Libraries and Variables
#===============================================================

# Data Files Required:
# 1. onemap_subzone (shp folder)
# 2. skyrise_greenery (shp folder)
# 3. total planning area population 2019 (csv) [total_pop_planningarea2019.csv -> for population density calculation]
# 4. skyrise_hdb (shp folder)

library(rgdal) 
library(maptools) 
library(raster)
library(sf)
library(spatstat)
library(tmap)
library(raster)
library(units) # change area units to km

source("4_weather_interpolation.R") # to retrieve temp&rainfall interpolation
source("constants.R")


# Loading and Preparing Data
#===============================================================
# Upload data & manipulate it to have required info
skyrise_greenery.csr <- readOGR("data/skyrise_greenery")
skyrise_greenery.csr@coords <- skyrise_greenery.csr@coords[, 1:2] # Drop Z coord
skyrise_greenery.csr@data <- skyrise_greenery.csr@data[,-(1:4)] # Make it as unmarked points
sf_skyrise_greenery.csr <- st_as_sf(skyrise_greenery.csr)
sf_skyrise_greenery.csr <- st_transform(sf_skyrise_greenery.csr, crs= 3414)

sg_subzone <- readOGR("data/onemap_subzone")
sf_subzone <- st_make_valid(st_as_sf(sg_subzone))
row.names(sf_subzone) <- NULL
sf_subzone <- st_transform(sf_subzone, crs=3414)
sf_planning_area <- sf_subzone %>% group_by(pln_area) %>% summarise(geometry = st_union(geometry))

planningarea_pop <- read.csv("data/total_pop_planningarea2019.csv")
# add pop to sf
sf_planning_area <- merge(sf_planning_area, planningarea_pop, by.x='pln_area', by.y='Planning_area')
# add area to sf
sf_planning_area$area <- st_area(sf_planning_area)
sf_planning_area$area_km <- set_units(sf_planning_area$area, "km^2")
# calculate pop density (in km^2)
sf_planning_area$pop_density <- sf_planning_area$Population / sf_planning_area$area_km

hdb_skyrise.csr <- readOGR("data/skyrise_hdb")
hdb_skyrise.csr@coords <- hdb_skyrise.csr@coords[, 1:2] # Drop Z coord
hdb_skyrise.csr@data <- hdb_skyrise.csr@data[,-(1:32)]
sf_hdb_skyrise_greenery.csr <- st_as_sf(hdb_skyrise.csr)
sf_hdb_skyrise_greenery.csr <- st_transform(sf_hdb_skyrise_greenery.csr, crs= 3414)


# Rasterize population density for hypothesis test
pop_den.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_planning_area)) 
pop_den.r <- rasterize(sf_planning_area, pop_den.r, field = "pop_density")

crs(pop_den.r) <- crs(sf_planning_area)
tm_shape(pop_den.r) + tm_raster(title = "Population density by planning area") 


# EDA - population density
#===============================================================
# Generate choropleth map for population density
tm_shape(sf_planning_area) + tm_polygons("pop_density", title="Population Density (km^2)") +
  tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.05) + tm_borders(col="grey", alpha=0.5) +
  tm_shape(sf_hdb_skyrise_greenery.csr) + tm_symbols(col="darkgreen", size=0.3, alpha=0.6) +
  tm_layout(
    title = "Population Density by Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, 
    title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, 
    inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 0.8, text.size = 0.7) +
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type=tm_compass.type, position=tm_compass.position, 
             show.labels=tm_compass.show.labels, size=tm_compass.size)


# Hypothesis Test 
#===============================================================
# Converting data for use in hypo test
pop_den.im <- as.im(pop_den.r)
skyrise_ppp <- as.ppp(sf_skyrise_greenery.csr)
planning_area.owin <- as.owin(sf_planning_area)
r_temp.im <- as.im(r_temp.m) #from temp interpolation
r_rainfall.im <- as.im(r_rainfall.m) #from rainfall interpolation

# Rescale to be based on km
skyrise_ppp.km <- rescale(skyrise_ppp, 1000, "km")
pop_den.im.km    <- rescale(pop_den.im, 1000, "km")
planning_area.owin.km <- rescale(planning_area.owin, 1000, "km")
r_temp.im.km    <- rescale(r_temp.im, 1000, "km")
r_rainfall.im.km    <- rescale(r_rainfall.im, 1000, "km")


ann.p <- mean(nndist(skyrise_ppp.km, k=1)) 
ann.p #avg nn distance is 379.6014m

## ---------- Running CSR ------------ ##
n <- 599L 
# Null Hypothesis (Base model) - location of skyrise greenery consistent with CSR
ann.r <- vector(length = n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_ppp.km$n, win=planning_area.owin.km) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
} 

plot(rand.p, pch=16, main="H0: Skyrise greenery follows CSR", cols=rgb(0,0,0,0.5))
# Histogram of simulated KNN values
hist(ann.r, main="H0: Skyrise greenery follows CSR", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.1, ann.r))
abline(v=ann.p, col="blue")
# Interpretation: For CSR skyrise would be distributed with dis ranging btw 700-800m


# Alternative Hypothesis 1 - with the influence of pop density
ann.r_alt1 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h1 <- rpoint(n=skyrise_ppp.km$n, f=pop_den.im.km, win=planning_area.owin.km)
  ann.r_alt1[i] <- mean(nndist(rand.p.h1, k=1)) 
} 
Window(rand.p.h1) <- planning_area.owin.km
plot(rand.p.h1, pch=16, main="Hypothesis Testing (Population Density)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt1, main="Hypothesis Testing (Population Density)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.05, ann.r_alt1+0.05))
abline(v=ann.p, col="blue")

#p-val
N.greater <- sum(ann.r_alt1/1000 > ann.p) 
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1) 
p #0.001666667


## Alternative Hypothesis 2 - with the influence of temperature
ann.r_alt2 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h2 <- rpoint(n=skyrise_ppp.km$n, f=r_temp.im.km, win=planning_area.owin.km)
  ann.r_alt2[i] <- mean(nndist(rand.p.h2, k=1)) 
} 
Window(rand.p.h2) <- planning_area.owin.km
plot(rand.p.h2, pch=16, main="Hypothesis Testing (Temperature)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt2, main="Hypothesis Testing (Temperature)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.1, ann.r_alt2+0.1))
abline(v=ann.p, col="blue")

N.greater2 <- sum(ann.r_alt2 > ann.p) 
p2 <- min(N.greater2 + 1, n + 1 - N.greater2) / (n +1) 
p2


## Alternative Hypothesis 3 - with the influence of rainfall
ann.r_alt3 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h3 <- rpoint(n=skyrise_ppp.km$n, f=r_rainfall.im.km, win=planning_area.owin.km)
  ann.r_alt3[i] <- mean(nndist(rand.p.h3, k=1)) 
} 
Window(rand.p.h3) <- planning_area.owin.km
plot(rand.p.h3, pch=16, main="Hypothesis Testing (Rainfall)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt3, main="Hypothesis Testing (Rainfall)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.1, ann.r_alt3+0.1))
abline(v=ann.p, col="blue")

N.greater3 <- sum(ann.r_alt3/1000 > ann.p) 
p3 <- min(N.greater3 + 1, n + 1 - N.greater3) / (n +1) 
p3


##### ----- Point Poisson & ANOVA ----- #####
#===============================================================
ppm_h0 <- ppm(skyrise_ppp.km ~ 1) #H0: CSR
ppm_h0

ppm_1 <- ppm(skyrise_ppp.km ~ pop_den.im.km) #H1
ppm_1

# add temp
ppm_2 <- ppm(skyrise_ppp.km ~ pop_den.im.km + r_temp.im.km)
ppm_2

# add rainfall
ppm_3 <- ppm(skyrise_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km)
ppm_3


## With one variable only
ppm_popden <- ppm_1
ppm_popden

ppm_temp <- ppm(skyrise_ppp.km ~ r_temp.im.km)
ppm_temp

ppm_rainfall <- ppm(skyrise_ppp.km ~ r_rainfall.im.km)
ppm_rainfall

#===============================================================
anova(ppm_h0, ppm_1, ppm_2, ppm_3, test="LRT")
anova(ppm_h0, ppm_popden, ppm_temp, ppm_rainfall, test="LRT")

anova(ppm_h0, ppm_1, test="LRT")
anova(ppm_h0, ppm_2, test="LRT")
anova(ppm_h0, ppm_3, test="LRT")

## With one variable only
anova(ppm_h0, ppm_popden, test="LRT")
anova(ppm_h0, ppm_temp, test="LRT")
anova(ppm_h0, ppm_rainfall, test="LRT")

