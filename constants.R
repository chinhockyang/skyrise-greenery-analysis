# Constant Variables and Functions (Utility File)

# Load Libraries and Variables
#===============================================================
library(fMultivar)

# tm_style
#===============================================================
global.fontfamily = "Palatino"

# options: "gray", "natural", "cobalt", "col_blind", "albatross", "beaver", "bw", "classic", "watercolor", "red", "black" 
global.style = "col_blind"

# tm_layout
tm_layout.title.size = 1.5
tm_layout.title.position = c("left", "top")
tm_layout.title.fontface = "bold"
tm_layout.title.fontfamily = global.fontfamily
tm_layout.inner.margins = c(-0.05,0.05,0.1,0.03)

# tm_compass
tm_compass.type = "rose"
tm_compass.position = c("right", "top")
tm_compass.show.labels = 3
tm_compass.size = 2


# Point Pattern Analysis
#===============================================================
# For KDE
choose_bw <- function(spdf) { 
  X <- st_coordinates(spdf) 
  sigma <- c(sd(X[,1]),sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6) 
  return(sigma/1000)
}

# For hexagon binning
hexbin_map <- function(spdf, ...) {
  hbins <- fMultivar::hexBinning(coordinates(spdf), ...)
  u <- c(1, 0, -1, -1, 0, 1)
  u <- u * min(diff(unique(sort(hbins$x)))) 
  v <- c(1,2,1,-1,-2,-1) 
  v <- v * min(diff(unique(sort(hbins$y))))/3
  
  # Construct each polygon in the sp model 
  hexes_list <- vector(length(hbins$x),mode='list')
  
  for (i in 1:length(hbins$x)) { 
    pol <- Polygon(cbind(u + hbins$x[i], v + hbins$y[i]),hole=FALSE)
    hexes_list[[i]] <- Polygons(list(pol),i)
  }
  
  # Build the spatial polygons data frame 
  hex_cover_sp <- SpatialPolygons(hexes_list,proj4string=CRS(proj4string(spdf)))
  hex_cover <- SpatialPolygonsDataFrame(hex_cover_sp, data.frame(z=hbins$z),match.ID=FALSE)
  return(hex_cover)
}