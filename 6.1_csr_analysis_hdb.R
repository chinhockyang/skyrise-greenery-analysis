# Complete Spatial Randomness Study (HDB Skyrise Greenery)

# Load Libraries and Variables
#===============================================================

# Data Files Required:
# 1. onemap_subzone (shp folder)
# 2. skyrise_hdb (shp folder)
# 3. planning area population for density calculation (csv) [total_pop_planningarea2019.csv]
# 4. hdb_info_across_planning_area (shp folder)
# 5. hdb_prices_pln_area (shp folder)
# 6. aedes_info_across_planning_area (shp folder)

library(rgdal) 
library(maptools) 
library(raster)
library(sf)
library(spatstat)
library(tmap)
library(raster)
library(units)

source("4_weather_interpolation.R") # to retrieve temp&rainfall interpolation
source("6_csr_analysis.R") # to re-use the im files for hypo testing
source("constants.R")

hdb_resale_prices <- readOGR("data/hdb_prices_pln_area")
sf_hdb_resale_prices <- st_make_valid(st_as_sf(hdb_resale_prices))
sf_hdb_resale_prices <- st_transform(st_as_sf(hdb_resale_prices), crs=3414)
#sf_hdb_resale_prices$rsl_prc[is.na(sf_hdb_resale_prices$rsl_prc)] <- mean(sf_hdb_resale_prices$rsl_prc, na.rm = TRUE)
sf_hdb_resale_prices$rsl_prc_thousands <- sf_hdb_resale_prices$rsl_prc/1000 

hdb_info_across_planning_area <- readOGR("data/hdb_info_across_planning_area")
hdb_info_across_planning_area@data[is.na(hdb_info_across_planning_area@data)] = 0 # replace na values on floor with 0
sf_hdb_info_across_planning_area <- st_make_valid(st_as_sf(hdb_info_across_planning_area))
sf_hdb_info_across_planning_area$year[sf_hdb_info_across_planning_area$year==0] <- 2019
sf_hdb_info_across_planning_area$dur_existed <- 2019-sf_hdb_info_across_planning_area$year

hdb_skyrise.csr <- readOGR("data/skyrise_hdb")
hdb_skyrise.csr@coords <- hdb_skyrise.csr@coords[, 1:2] # Drop Z coord
hdb_skyrise.csr@data <- hdb_skyrise.csr@data[,-(1:32)]
sf_hdb_skyrise_greenery.csr <- st_as_sf(hdb_skyrise.csr)
sf_hdb_skyrise_greenery.csr <- st_transform(sf_hdb_skyrise_greenery.csr, crs= 3414)
#head(sf_hdb_skyrise_greenery.csr)

aedes_hotspots <- readOGR("data/aedes_info_across_planning_area")
sf_aedes_hotspots <- st_make_valid(st_as_sf(aedes_hotspots))
sf_aedes_hotspots <- st_transform(st_as_sf(aedes_hotspots), crs=3414)


# Rasterize mean HDB floor for hypo test
hdb_floor.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_hdb_info_across_planning_area)) 
hdb_floor.r <- rasterize(sf_hdb_info_across_planning_area, hdb_floor.r, field = "floor")
crs(hdb_floor.r) <- crs(sf_hdb_info_across_planning_area) 
# Raster visualisation
tm_shape(hdb_floor.r) + tm_raster(title = "Mean Floor by planning area") 

# Rasterize completion year for hypo test
hdb_completion.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_hdb_info_across_planning_area)) 
hdb_completion.r <- rasterize(sf_hdb_info_across_planning_area, hdb_completion.r, field = "dur_existed")
crs(hdb_completion.r) <- crs(sf_hdb_info_across_planning_area) 
# Raster visualisation
tm_shape(hdb_completion.r) + tm_raster(title = "Years Present") 

# Rasterize resale price for hypo test
hdb_resale_px.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_hdb_resale_prices)) 
hdb_resale_px.r <- rasterize(sf_hdb_resale_prices, hdb_resale_px.r, field = "rsl_prc_thousands")
crs(hdb_resale_px.r) <- crs(sf_hdb_resale_prices) 
# Raster visualisation
tm_shape(hdb_resale_px.r) + tm_raster(title = "HDB Resale Price by planning area ('000)") 

# Rasterize Aedes Hotspot for hypo test
aedeshotspot.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_aedes_hotspots)) 
aedeshotspot.r <- rasterize(sf_aedes_hotspots, aedeshotspot.r, field = "hotspots")
crs(aedeshotspot.r) <- crs(sf_aedes_hotspots) 
# Raster visualisation
tm_shape(aedeshotspot.r) + tm_raster(title = "Aedes Hotspot") 


# EDA - population density
#===============================================================
# Generate choropleth map for mean height by floors of HDBs
tm_shape(sf_hdb_info_across_planning_area) + tm_polygons("floor", title="Mean floor of HDBs") +
  tm_layout(
    title = "Mean Floor of HDBs by Planning Area", 
    title.size = tm_layout.title.size, title.position=tm_layout.title.position, 
    title.fontface=tm_layout.title.fontface, title.fontfamily=tm_layout.title.fontfamily, 
    inner.margins=tm_layout.inner.margins
  ) +  
  tm_legend(position=c("right", "bottom"), title.size = 0.7, text.size = 0.6) +
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type=tm_compass.type, position=tm_compass.position, 
             show.labels=tm_compass.show.labels, size=1.5) +
  tm_shape(sf_planning_area) + tm_fill(col="white", alpha = 0.05) + tm_borders(col="grey", alpha=0.5) 




# Hypothesis Test 
#===============================================================
# Converting data for use in hypo test
skyrise_hdb_ppp <- as.ppp(sf_hdb_skyrise_greenery.csr)
planning_area_hdb.owin <- as.owin(sf_hdb_info_across_planning_area)
hdb_floor.im <- as.im(hdb_floor.r)
hdb_resale_px.im <- as.im(hdb_resale_px.r)
hdb_completion.im <- as.im(hdb_completion.r)
aedeshotspot.im <- as.im(aedeshotspot.r)
# Obtained from 6_csr_analysis.R file
#pop_den.im <- as.im(pop_den.r)
#r_temp.im <- as.im(r_temp.m) #from temp interpolation
#r_rainfall.im <- as.im(r_rainfall.m) #from rainfall interpolation


# Rescale to be based on km
skyrise_hdb_ppp.km <- rescale(skyrise_hdb_ppp, 1000, "km")
planning_area_hdb.owin.km <- rescale(planning_area_hdb.owin, 1000, "km")
hdb_floor.im.km <- rescale(hdb_floor.im, 1000, "km")
hdb_resale_px.im.km <- rescale(hdb_resale_px.im, 1000, "km")
aedeshotspot.im.km <- rescale(aedeshotspot.im, 1000, "km")
hdb_completion.im.km <- rescale(hdb_completion.im, 1000, "km")
# Obtained from 6_csr_analysis.R file
#pop_den.im.km <- rescale(pop_den.im, 1000, "km")
#r_temp.im.km <- rescale(r_temp.im, 1000, "km")
#r_rainfall.im.km <- rescale(r_rainfall.im, 1000, "km")

ann.p <- mean(nndist(skyrise_hdb_ppp.km, k=1)) 
ann.p #0.3827644km


## ---------- Running CSR ------------ ##
n <- 599L 
# Null Hypothesis (Base model) - location of HDB skyrise greenery consistent with CSR
ann.hdb.r <- vector(length = n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_hdb_ppp.km$n, win=planning_area_hdb.owin.km) 
  ann.hdb.r[i] <- mean(nndist(rand.p, k=1))
} 

plot(rand.p, pch=16, main="H0: HDB Skyrise greenery follows CSR", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r, main="H0: Skyrise greenery follows CSR", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.15, ann.hdb.r+0.1))
abline(v=ann.p, col="blue")
# Interpretation: For CSR skyrise would be distributed with dis ranging btw 1000-1500m


## Alternative Hypothesis 1 - with the influence of pop density
ann.hdb.r_alt1 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h1 <- rpoint(n=skyrise_hdb_ppp.km$n, f=pop_den.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt1[i] <- mean(nndist(rand.p.h1, k=1)) 
} 
Window(rand.p.h1) <- planning_area_hdb.owin.km
plot(rand.p.h1, pch=16, main="Hypothesis Testing (Population Density)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt1, main="Hypothesis Testing (Population Density)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.1, ann.hdb.r_alt1))
abline(v=ann.p, col="blue")

#p-val
N.greater <- sum(ann.hdb.r_alt1 > ann.p) 
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1) 
p #0.001666667


## Alternative Hypothesis 2 - with the influence of temperature
ann.hdb.r_alt2 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h2 <- rpoint(n=skyrise_hdb_ppp.km$n, f=r_temp.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt2[i] <- mean(nndist(rand.p.h2, k=1)) 
} 
Window(rand.p.h2) <- planning_area_hdb.owin.km
plot(rand.p.h2, pch=16, main="Hypothesis Testing (Temperature)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt2, main="Hypothesis Testing (Temperature)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.15, ann.hdb.r_alt2))
abline(v=ann.p, col="blue")

N.greater2 <- sum(ann.hdb.r_alt2 > ann.p) 
p2 <- min(N.greater2 + 1, n + 1 - N.greater2) / (n +1) 
p2


## Alternative Hypothesis 3 - with the influence of rainfall
ann.hdb.r_alt3 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h3 <- rpoint(n=skyrise_hdb_ppp.km$n, f=r_rainfall.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt3[i] <- mean(nndist(rand.p.h3, k=1)) 
} 
Window(rand.p.h3) <- planning_area_hdb.owin.km
plot(rand.p.h3, pch=16, main="Hypothesis Testing (Rainfall)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt3, main="Hypothesis Testing (Rainfall)", las=1, breaks=40, col="bisque", xlim=range(ann.p-0.15, ann.hdb.r_alt3+0.1))
abline(v=ann.p, col="blue")

N.greater3 <- sum(ann.hdb.r_alt3 > ann.p) 
p3 <- min(N.greater3 + 1, n + 1 - N.greater3) / (n +1) 
p3


## Alternative Hypothesis 4 - with the hdb height (floors)
ann.hdb.r_alt4 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h4 <- rpoint(n=skyrise_hdb_ppp.km$n, f=hdb_floor.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt4[i] <- mean(nndist(rand.p.h4, k=1)) 
} 
Window(rand.p.h4) <- planning_area_hdb.owin.km
plot(rand.p.h4, pch=16, main="Hypothesis Testing (HDB Floors)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt4, main="Hypothesis Testing (HDB Floors)", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.hdb.r_alt4))
abline(v=ann.p, col="blue")

N.greater4 <- sum(ann.hdb.r_alt4/1000 > ann.p) 
p4 <- min(N.greater4 + 1, n + 1 - N.greater4) / (n +1) 
p4


## Alternative Hypothesis 5 - with hdb resale prices
ann.hdb.r_alt5 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h5 <- rpoint(n=skyrise_hdb_ppp.km$n, f=hdb_resale_px.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt5[i] <- mean(nndist(rand.p.h5, k=1)) 
} 
Window(rand.p.h5) <- planning_area_hdb.owin.km
plot(rand.p.h5, pch=16, main="Hypothesis Testing (HDB Resale Prices)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt5, main="Hypothesis Testing (HDB Resale Prices)", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.hdb.r_alt5))
abline(v=ann.p, col="blue")

N.greater5 <- sum(ann.hdb.r_alt5/1000 > ann.p) 
p5 <- min(N.greater5 + 1, n + 1 - N.greater5) / (n +1) 
p5


## Alternative Hypothesis 6 - with number of aedes hotspot in planning area
ann.hdb.r_alt6 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h6 <- rpoint(n=skyrise_hdb_ppp.km$n, f=aedeshotspot.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt6[i] <- mean(nndist(rand.p.h6, k=1)) 
} 
Window(rand.p.h6) <- planning_area_hdb.owin.km
plot(rand.p.h6, pch=16, main="Hypothesis Testing (Aedes Hotspot)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt6, main="Hypothesis Testing (Aedes Hotspot)", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.hdb.r_alt6))
abline(v=ann.p, col="blue")

N.greater6 <- sum(ann.hdb.r_alt6/1000 > ann.p) 
p6 <- min(N.greater6 + 1, n + 1 - N.greater6) / (n +1) 
p6


## Alternative Hypothesis 7 - with avg hdb completion year
ann.hdb.r_alt7 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h7 <- rpoint(n=skyrise_hdb_ppp.km$n, f=hdb_completion.im.km, win=planning_area_hdb.owin.km)
  ann.hdb.r_alt7[i] <- mean(nndist(rand.p.h7, k=1)) 
} 
Window(rand.p.h7) <- planning_area_hdb.owin.km
plot(rand.p.h7, pch=16, main="Hypothesis Testing (HDB Age)", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.hdb.r_alt7, main="Hypothesis Testing (HDB Age)", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.hdb.r_alt7))
abline(v=ann.p, col="blue")

N.greater7 <- sum(ann.hdb.r_alt7/1000 > ann.p) 
p7 <- min(N.greater7 + 1, n + 1 - N.greater7) / (n +1) 
p7


##### ----- Point Poisson & ANOVA ----- #####
hdb_ppm_h0 <- ppm(skyrise_hdb_ppp.km ~ 1) #H0: CSR
hdb_ppm_h0

# add population density
hdb_ppm_1 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km)
hdb_ppm_1

# temp
hdb_ppm_2 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km)
hdb_ppm_2

# add rainfall
hdb_ppm_3 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km)
hdb_ppm_3

# add hdb floors
hdb_ppm_4 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km + hdb_floor.im.km)
hdb_ppm_4

# add hdb resale price
hdb_ppm_5 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km + hdb_floor.im.km + hdb_resale_px.im.km)
hdb_ppm_5

# add aedes hotspots
hdb_ppm_6 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km + 
                   hdb_floor.im.km + hdb_resale_px.im.km + aedeshotspot.im.km)
hdb_ppm_6

# add hdb completion
hdb_ppm_7 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km + 
                   hdb_floor.im.km + hdb_resale_px.im.km + aedeshotspot.im.km + hdb_completion.im.km)
hdb_ppm_7


## With one variable only
hdb_ppm_popden <- hdb_ppm_1
hdb_ppm_popden

hdb_ppm_temp <- ppm(skyrise_hdb_ppp.km ~ r_temp.im.km)
hdb_ppm_temp

hdb_ppm_rainfall <- ppm(skyrise_hdb_ppp.km ~ r_rainfall.im.km)
hdb_ppm_rainfall

hdb_ppm_floor <- ppm(skyrise_hdb_ppp.km ~ hdb_floor.im.km)
hdb_ppm_floor

hdb_ppm_price <- ppm(skyrise_hdb_ppp.km ~ hdb_resale_px.im.km)
hdb_ppm_price

hdb_ppm_aedes <- ppm(skyrise_hdb_ppp.km ~ aedeshotspot.im.km)
hdb_ppm_aedes

hdb_ppm_age <- ppm(skyrise_hdb_ppp.km ~ hdb_completion.im.km)
hdb_ppm_age

#===============================================================
# ANOVA Test
anova(hdb_ppm_h0, hdb_ppm_1, hdb_ppm_2, hdb_ppm_3, hdb_ppm_4, hdb_ppm_5, hdb_ppm_6, hdb_ppm_7, test="LRT")

anova(hdb_ppm_h0, hdb_ppm_1, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_2, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_3, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_4, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_5, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_6, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_7, test="LRT")

## With one variable only
anova(hdb_ppm_h0, hdb_ppm_popden, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_temp, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_rainfall, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_floor, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_price, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_aedes, test="LRT")
anova(hdb_ppm_h0, hdb_ppm_age, test="LRT")





