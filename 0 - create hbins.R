# Create hbins at specified size (w, h) for specified n levels

# load packages
library(tidyverse)
library(sf)
library(glue)

# load AOI and project to web meractor
aoi <- st_read("../ArcGIS/GreaterLondon.geojson")
aoi <- st_transform(aoi, 3857)

# specify hbin parameters
## specify max cellsize
max_size <- 48000
## specify number of levels
nlevels <- 7

# initial calculations
## get lower left corner of aoi bounding box
min_xy <- st_bbox(aoi)[c(1, 2)]
## calculate bounding box width
dx <- diff(st_bbox(aoi)[c(1,3)])
## calculate bounding box height
dy <- diff(st_bbox(aoi)[c(2,4)])
## calculate center of bounding box
center <- c(((dx*0.5) + min_xy[1]), ((dy*0.5) + min_xy[2]))
## number of cells in x and y directions at max_size
n <- c(ceiling(dx/max_size), ceiling(dy/max_size))
## calculate lower left corner of grid
grid_origin <- c(center[1]-(max_size*n[1])*0.5, center[2]-(max_size*n[2])*0.5)

# print levels and cell sizes based on input
for (level in 1:nlevels) {
  power <- 2^(nlevels-(level))
  cellsize <- max_size/power
  ncols <- ceiling(dx/max_size)*(power)
  nrows <- ceiling(dy/max_size)*(power)
  print(glue("level: {level}, cellsize: {cellsize}, power: {power}, ncols: {ncols}, nrows: {nrows}"))
}

# define function to create hbins based on inputs
create_grid_level <- function(level) {
  #power <- 2^(nlevels-(level-1))
  #cellsize <- max_size/power*2
  power <- 2^(nlevels-(level))
  cellsize <- max_size/power
  ncols <- ceiling(dx/max_size)*(power)
  nrows <- ceiling(dy/max_size)*(power)
  hbins_level <- st_make_grid(
    aoi, 
    n=c(ncols, nrows),
    cellsize=cellsize,
    offset=grid_origin
  )
  return(hbins_level %>% as_tibble() %>% mutate(level = level))
}

# create hbins for all levels
hbins <- map_df(1:nlevels, create_grid_level)

# plot resulting features
## plot aoi
plot(aoi$geometry)
## plot highest level
plot(filter(hbins, level == nlevels)$geometry, add = TRUE)

# save resulting features
st_write(hbins, "../results/R5 - London 750m 7L/hbins_750m_7L.geojson")