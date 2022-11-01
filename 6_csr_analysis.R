library(rgdal) 
library(maptools) 
library(raster)
library(sf)
library(spatstat)
library(tmap)
library(ggplot2)
library(raster)
library(units) # change area units to km

source("4_weather_interpolation.R") # to retrieve temp&rainfall interpolation

##### ----- upload data & manipulate it to have required info ----- ####
skyrise_greenery.csr <- readOGR("data/skyrise_greenery")
skyrise_greenery.csr@coords <- skyrise_greenery.csr@coords[, 1:2] # Drop Z coord
skyrise_greenery.csr@data <- skyrise_greenery.csr@data[,-(1:4)]
sf_skyrise_greenery.csr <- st_as_sf(skyrise_greenery.csr)
sf_skyrise_greenery.csr <- st_transform(sf_skyrise_greenery.csr, crs= 3414)


hdb_prices_with_planning_area <- readOGR("data/hdb_prices_with_planning_area")
sf_hdb_prices_with_planning_area <- st_make_valid(st_as_sf(hdb_prices_with_planning_area))
sf_hdb_prices_with_planning_area <- st_transform(st_as_sf(hdb_prices_with_planning_area), crs=3414)
#tm_shape(sf_hdb_prices_with_planning_area) + tm_dots(title = "HDB Plot") 
# grp by planning area, get avg of price


sg_planning_area <- readOGR("data/onemap_planning_area")
sf_planning_area <- st_make_valid(st_as_sf(sg_planning_area))
sf_planning_area <- st_transform(st_as_sf(sg_planning_area), crs=3414)

planningarea_pop <- read.csv("data/total_pop_planningarea2019.csv")
# add pop to sf
sf_planning_area <- merge(sf_planning_area, planningarea_pop, by.x='pln_area_n', by.y='Planning_area')
# add area to sf
sf_planning_area$area <- st_area(sf_planning_area)
sf_planning_area$area_km <- set_units(sf_planning_area$area, "km^2")
# calculate pop density (in km^2)
sf_planning_area$pop_density <- sf_planning_area$Population / sf_planning_area$area_km


##### ----- rasterize population density for hypo test ----- #####
pop_den.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_planning_area)) 
pop_den.r <- rasterize(sf_planning_area, pop_den.r, field = "pop_density")

crs(pop_den.r) <- crs(sf_planning_area) 

tm_shape(pop_den.r) + tm_raster(title = "Population density by planning area") 


##### ----- Hypo test ----- #####
# converting data for use in hypo test
pop_den.im <- as.im(pop_den.r)
skyrise_ppp <- as.ppp(sf_skyrise_greenery.csr)
planning_area.owin <- as.owin(sf_planning_area)
r_temp.im <- as.im(r_temp.m) #from temp interpolation
r_rainfall.im <- as.im(r_rainfall.m) #from rainfall interpolation

# rescale to be based on km
skyrise_ppp.km <- rescale(skyrise_ppp, 1000, "km")
pop_den.im.km    <- rescale(pop_den.im, 1000, "km")
planning_area.owin.km <- rescale(planning_area.owin, 1000, "km")
r_temp.im.km    <- rescale(r_temp.im, 1000, "km")
r_rainfall.im.km    <- rescale(r_rainfall.im, 1000, "km")

ann.p <- mean(nndist(skyrise_ppp.km, k=1)) 
ann.p #avg nn distance is 379.6014m

## Null Hypothesis CSR - randomly located ard SG
n <- 599L 
ann.r <- vector(length = n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_ppp.km$n, win=planning_area.owin.km) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
} 

plot(rand.p, pch=16, main="H0: Skyrise greenery follows CSR", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r, main="H0: Skyrise greenery follows CSR", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")
# Interpretation: For CSR skyrise would be distributed with dis ranging btw 700-800m

## Alternative Hypothesis 1 - with the influence of pop density
ann.r_alt <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h1 <- rpoint(n=skyrise_ppp.km$n, f=pop_den.im.km, win=planning_area.owin.km)
  ann.r_alt[i] <- mean(nndist(rand.p.h1, k=1)) 
} 
Window(rand.p.h1) <- planning_area.owin.km
plot(rand.p.h1, pch=16, main="H1", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt, main="H1", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r_alt))
abline(v=ann.p, col="blue")

#p-val
N.greater <- sum(ann.r_alt/1000 > ann.p) 
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1) 
p #0.001666667

## Alternative Hypothesis 2 - with the influence of temperature
ann.r_alt2 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h2 <- rpoint(n=skyrise_ppp.km$n, f=r_temp.im.km, win=planning_area.owin.km)
  ann.r_alt2[i] <- mean(nndist(rand.p.h2, k=1)) 
} 
Window(rand.p.h2) <- planning_area.owin.km
plot(rand.p.h2, pch=16, main="H2", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt2, main="Alt Hypothesis (H2): Temperature", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r_alt2))
abline(v=ann.p, col="blue")

N.greater2 <- sum(ann.r_alt2/1000 > ann.p) 
p2 <- min(N.greater2 + 1, n + 1 - N.greater2) / (n +1) 
p2


## Alternative Hypothesis 3 - with the influence of rainfall
ann.r_alt3 <- vector(length=n) 
for (i in 1:n){ 
  rand.p.h3 <- rpoint(n=skyrise_ppp.km$n, f=r_rainfall.im.km, win=planning_area.owin.km)
  ann.r_alt3[i] <- mean(nndist(rand.p.h3, k=1)) 
} 
Window(rand.p.h3) <- planning_area.owin.km
plot(rand.p.h3, pch=16, main="H3", cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt3, main="Alt Hypothesis (H3): Rainfall", las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r_alt3))
abline(v=ann.p, col="blue")

N.greater3 <- sum(ann.r_alt3/1000 > ann.p) 
p3 <- min(N.greater3 + 1, n + 1 - N.greater3) / (n +1) 
p3


##### ----- Point Poisson & ANOVA ----- #####
ppm_h0 <- ppm(skyrise_ppp.km ~ 1) #H0: not a fn of pop density
ppm_h0

ppm_h1 <- ppm(skyrise_ppp.km ~ pop_den.im.km) #H1
ppm_h1

# h2 - add temp
ppm_h2 <- ppm(skyrise_ppp.km ~ pop_den.im.km + r_temp.im.km)
ppm_h2

# h3 - add rainfall
ppm_h3 <- ppm(skyrise_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km)
ppm_h3

anova(ppm_h0, ppm_h1, ppm_h2, ppm_h3, test="LRT")



#potential h2 - add prices?
# ppm_h2 <- ppm(skyrise_ppp.km~pop_den.im.km) #H1
#ppm_h2




#do another on hdb version
# get interpolated temp & rainfall data as h2
