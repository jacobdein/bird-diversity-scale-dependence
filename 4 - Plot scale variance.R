# Plots scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)
library(scales)
library(grid)

source("./utilities/shift_legend.R")

# specify analysis run name
run <- "R3 - London 500m 9L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
sve <- st_read(file.path("../results", run, "geojson/sve.geojson"))

# plot data
p <- ggplot() + 
  geom_sf(data = sve %>% filter(level < 9), aes(fill = sv), color=NA) +
  scale_fill_gradientn(colors=c('white', 'green'), limits=c(0,200), oob=squish) + 
  facet_wrap(facets = vars(level), nrow=3, ncol=3) +
  theme_minimal() +
  theme(
    panel.background = element_rect(colour = "lightgray", linewidth = 1)
    #panel.border = element_rect(colour = "lightgray", linewidth = 1)
  )

grid.draw(shift_legend(p))

