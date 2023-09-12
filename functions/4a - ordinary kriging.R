# Krige estimated diversity over AOI

# load packages
library(tidyverse)
library(gstat)
library(sf)

execute_OK <- function(hbins_estD, hbins, lag_size) {
  
  # prepare observation points used to develop kriging model
  obs_locations <- hbins_estD %>% 
    filter(level==1) %>%
    st_centroid() %>%
    bind_cols(
      st_coordinates(.$geometry)
    ) %>%
    drop_na(estD)
  
  # prepare points to predict values using kriging model
  pred_locations <- hbins %>% 
    filter(level==1) %>%
    st_centroid()
  
  # calculate variogram
  var <- variogram(estD ~ 1, 
                   data = obs_locations, 
                   locations = as.data.frame(st_coordinates(obs_locations$geometry)),
                   width = lag_size)
  
  # fit variogram
  fvm <- fit.variogram(var, vgm("Exc"))
  
  # execute ordinary kriging as implemented in gstat
  x <- krige(estD~1, locations = obs_locations, newdata = pred_locations, model = fvm)
  
  # join results to dataset
  OK_result <- sf::st_join(pred_locations, x)
  
  return(OK_result)
}