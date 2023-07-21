# Plots scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)
library(scales)
library(grid)
library(sfdep)

source("./utilities/shift_legend.R")

# specify analysis run name
run <- "R3 - London 500m 9L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
sve <- st_read(file.path("../results", run, "geojson/sve.geojson"))

# level 1 sve
sve4 <- sve %>% filter(level == 4)

weights <- sve4 %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb)
  )

lisa <- weights %>% 
  mutate(moran = local_moran(sv, nb, wt))


lisa %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) |> 
  ggplot(aes(fill = pysal)) +
  geom_sf() +
  geom_sf(lwd = 0.2, color = "black") +
  theme_void() +
  scale_fill_manual(values = c("#E01B1B", "#F0B8B1", "#1B53E0", "#B7D9E8"))






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

