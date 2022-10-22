library(rgdal) 
library(maptools) 
library(raster)
library(sf)
library(spatstat)
library(tmap)
library(ggplot2)
library(raster)
library(units) # change area units to km

##### ----- upload data & manipulate it to have required info ----- ####
skyrise_greenery <- readOGR("data/skyrise_greenery")
skyrise_greenery@coords <- skyrise_greenery@coords[, 1:2] # Drop Z coord
skyrise_greenery@data <- skyrise_greenery@data[,-(1:4)]
sf_skyrise_greenery <- st_as_sf(skyrise_greenery)
sf_skyrise_greenery <- st_transform(sf_skyrise_greenery, crs= 3414)


sg_planning_area <- readOGR("data/onemap_planning_area")
sf_sg_planning_area <- st_make_valid(st_as_sf(sg_planning_area))
sf_sg_planning_area <- st_transform(st_as_sf(sg_planning_area), crs=3414)

planningarea_pop <- read.csv("data/total_pop_planningarea2019.csv")
# add pop to sf
sf_planning_area <- merge(sf_sg_planning_area, planningarea_pop, by.x='pln_area_n', by.y='Planning_area')
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
skyrise_ppp <- as.ppp(sf_skyrise_greenery)
planning_area.owin <- as.owin(sf_planning_area)

# rescale to be based on km
skyrise_ppp.km <- rescale(skyrise_ppp, 1000, "km")
pop_den.im.km    <- rescale(pop_den.im, 1000, "km")
planning_area.owin.km <- rescale(planning_area.owin, 1000, "km")

ann.p <- mean(nndist(skyrise_ppp.km, k=1)) 
ann.p #avg nn distance is 379.6014m

## Null Hypothesis CSR - randomly located ard SG
n <- 599L 
ann.r <- vector(length = n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_ppp.km$n, win=planning_area.owin.km) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
} 

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")
# Interpretation: For CSR skyrise would be distributed with dis ranging btw 700-800m

## Alternative Hypothesis - with the influence of pop density
ann.r_alt <- vector(length=n) 
for (i in 1:n){ 
  rand.p <- rpoint(n=skyrise_ppp.km$n, f=pop_den.im.km, win=planning_area.owin.km)
  ann.r_alt[i] <- mean(nndist(rand.p, k=1)) 
} 
Window(rand.p) <- planning_area.owin.km
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))
#Histogram of simulated KNN values
hist(ann.r_alt, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r_alt))
abline(v=ann.p, col="blue")

#p-val
N.greater <- sum(ann.r_alt/1000 > ann.p) 
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1) 
p


##### ----- Point Poisson & ANOVA ----- #####
ppm_h0 <- ppm(skyrise_ppp.km~1) #H0: not a fn of pop density
ppm_h0

ppm_h1 <- ppm(skyrise_ppp.km~pop_den.im.km) #H1
ppm_h1

anova(ppm_h0, ppm_h1, test="LRT")




