library(rgdal) 
library(maptools) 
library(raster)
library(sf)
library(spatstat)
library(tmap)
library(ggplot2)
library(raster)
library(units)

source("4_weather_interpolation.R") # to retrieve temp&rainfall interpolation

#hdb_prices_with_planning_area <- readOGR("data/hdb_prices_with_planning_area")
#sf_hdb_prices_with_planning_area <- st_make_valid(st_as_sf(hdb_prices_with_planning_area))
#sf_hdb_prices_with_planning_area <- st_transform(st_as_sf(hdb_prices_with_planning_area), crs=3414)
#tm_shape(sf_hdb_prices_with_planning_area) + tm_dots(title = "HDB Plot") 
# grp by planning area, get avg of price

hdb_info_across_planning_area <- readOGR("data/hdb_info_across_planning_area")
sf_hdb_info_across_planning_area <- st_make_valid(st_as_sf(hdb_info_across_planning_area))
sf_hdb_info_across_planning_area <- st_transform(st_as_sf(hdb_info_across_planning_area), crs=3414)
head(hdb_info_across_planning_area)

hdb_skyrise.csr <- readOGR("data/skyrise_hdb")
hdb_skyrise.csr@coords <- hdb_skyrise.csr@coords[, 1:2] # Drop Z coord
hdb_skyrise.csr@data <- hdb_skyrise.csr@data[,-(1:32)]
sf_hdb_skyrise_greenery.csr <- st_as_sf(hdb_skyrise.csr)
#sf_hdb_skyrise_greenery.csr <- st_transform(hdb_skyrise.csr, crs= 3414)


##### ----- rasterize mean HDB floor for hypo test ----- #####
hdb_floor.r <- raster(nrow = 180, ncols = 360, ext = extent(sf_hdb_info_across_planning_area)) 
hdb_floor.r <- rasterize(hdb_info_across_planning_area, hdb_floor.r, field = "floor")

crs(hdb_floor.r) <- crs(sf_planning_area) 

tm_shape(sf_hdb_info_across_planning_area) + tm_polygons() +
tm_shape(hdb_floor.r) + tm_raster(title = "Mean Floor by planning area") 


##### ----- Hypo test ----- #####
# converting data for use in hypo test
pop_den.im <- as.im(pop_den.r)
skyrise_hdb_ppp <- as.ppp(sf_hdb_skyrise_greenery.csr)
planning_area.owin <- as.owin(sf_hdb_info_across_planning_area)
r_temp.im <- as.im(r_temp.m) #from temp interpolation
r_rainfall.im <- as.im(r_rainfall.m) #from rainfall interpolation

# rescale to be based on km
skyrise_hdb_ppp.km <- rescale(skyrise_hdb_ppp, 1000, "km")
pop_den.im.km    <- rescale(pop_den.im, 1000, "km")
planning_area.owin.km <- rescale(planning_area.owin, 1000, "km")
r_temp.im.km    <- rescale(r_temp.im, 1000, "km")
r_rainfall.im.km    <- rescale(r_rainfall.im, 1000, "km")



ann.p <- mean(nndist(skyrise_hdb_ppp.km, k=1)) 
ann.p



## Null Hypothesis CSR - randomly located ard SG
n <- 599L 
ann.r <- vector(length = n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_hdb_ppp.km$n, win=planning_area.owin.km) 
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
  rand.p.h1 <- rpoint(n=skyrise_hdb_ppp.km$n, f=pop_den.im.km, win=planning_area.owin.km)
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
  rand.p.h2 <- rpoint(n=skyrise_hdb_ppp.km$n, f=r_temp.im.km, win=planning_area.owin.km)
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
  rand.p.h3 <- rpoint(n=skyrise_hdb_ppp.km$n, f=r_rainfall.im.km, win=planning_area.owin.km)
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
ppm_h0 <- ppm(skyrise_hdb_ppp.km ~ 1) #H0: not a fn of pop density
ppm_h0

ppm_h1 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km) #H1
ppm_h1

# h2 - add temp
ppm_h2 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km)
ppm_h2

# h3 - add rainfall
ppm_h3 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km)
ppm_h3

# h4 - add hdb floors
ppm_h4 <- ppm(skyrise_hdb_ppp.km ~ pop_den.im.km + r_temp.im.km + r_rainfall.im.km + hdb_floor.im)
ppm_h4

anova(ppm_h0, ppm_h1, ppm_h2, ppm_h3, ppm_h4, test="LRT")



#potential h2 - add prices?
# ppm_h2 <- ppm(skyrise_hdb_ppp.km~pop_den.im.km) #H1
#ppm_h2








