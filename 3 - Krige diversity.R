# Krige estimated diversity across entire grid

# load packages
library(tidyverse)
library(gstat)
library(sp)
library(sf)
library(stars)
library(lattice)

# load data
hbins_estD <- readRDS("../results/bak/hbins_estD.rds")

df <- hbins_estD %>% 
  filter(level==1) %>%
  st_centroid() %>%
  bind_cols(
    st_coordinates(.$geometry)
  ) %>%
  drop_na(qD_2)
pred_xy <- hbins_estD %>% 
  filter(level==1) %>%
  st_centroid() #%>% 
  #st_coordinates(.$geometry) %>% 
  #as.data.frame()

pred_xy %>% drop_na(qD_2) %>% st_write("../results/R1 - London 1000m 5L/shp/estD_pts.shp")

var1 <- variogram(qD_2 ~ 1, data = df, locations = as.data.frame(st_coordinates(df$geometry)))

panel1 = function(x,y,...) { 
  vgm.panel.xyplot(x,y,...)
  panel.abline(h=var(df$qD_2), color = 'red')
}
plot(var1, panel = panel1)

fvm <- fit.variogram(var1, vgm("Sph"))
plot(fvm, cutoff = 30000)
x <- krige(qD_2~1, locations = df, newdata = pred_xy, model = fvm)

pred_coords <- pred_xy %>% 
  st_coordinates(.$geometry) %>% 
  as.data.frame()

# join results back to polygon geometry
x_poly <- x %>% data.frame()
x_poly$geometry <- hbins_estD %>% filter(level==1) %>% .$geometry
x_poly <- st_as_sf(x_poly)

ggplot() + 
  geom_sf(data = x_poly, aes(fill = var1.pred))
#plot(x_poly["var1.pred"])

# rasterize based on geometry and a column named "value". Change the name of this column if necessary
x_rast <-
  x_poly %>% 
  dplyr::select(var1.pred, geometry) %>% 
  st_rasterize()

# export as tiff
#write_stars(x_rast, "x_rast.tif")  

plot(x_rast, axes = TRUE, downsample = FALSE)
#levelplot(x_rast)
