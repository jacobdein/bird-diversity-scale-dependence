# Count and plot percent of grid cells containing both 1 and >=5 complete checklists

library(tidyverse)
library(sf)
source("./functions/2 - join hbin IDs.R")


# specify path to aoi geometry
run_aoi <- "../data/GreaterLondon.geojson"

# load eBird data
run_obs <- "../data/obs_London.rds"
obs <- read_rds(run_obs)

# project AOI to web meractor
aoi <- st_read(run_aoi)
aoi <- st_transform(aoi, 3857)

# specify grain sizes
grains <- c(50, 100, 250, 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)

# specify extent and calculate grid origin
extent <- 128000
max_size <- 64000

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

# define function to compute cell stats for a grid at a specified grain
get_stats <- function(grain) {
  
  ### 1 - Create grid ###
  grid <- st_make_grid(aoi, 
                       n=c(extent/grain, extent/grain),
                       cellsize=grain,
                       offset=grid_origin
  ) %>% 
    as_tibble() %>% 
    mutate(level = 1) %>% 
    st_as_sf()
  
  ### 2 - Join eBird observations to grid cells by IDs ###
  
  # join checklists to grid cells
  dataset <- join_hbins(grid, obs)
  
  ### 3 - Compute observation statistics for each grid cell ###
  # define function to calculate observation statistics in hbin
  compute_obs_statistics = function(binObs, id) {
    bin_summary <- tibble(
      id = id,
      #nObs = nrow(binObs),
      nLists = length(unique(binObs$sampling_event_identifier))
      #nSpecies = length(unique(binObs$common_name))
    )
    return(bin_summary)
  }
  
  # compute observation statistics for every hbin
  stats <- dataset$obs %>%
    select(common_name, sampling_event_identifier, level1) %>%
    st_drop_geometry() %>%
    split(x = ., f = .$level1) %>%
    map_dfr(~ compute_obs_statistics(binObs = .x, 
                                     id = unique(.x$level1))) %>%
    arrange(id)
  
  # return stats
  return(tibble(grain = grain, 
                n1 = stats %>% nrow() / grid %>% nrow(), 
                n5 = stats %>% filter(nLists >= 5) %>% nrow() / grid %>% nrow()))
}

# compute statistics for all specified grain sizes
results <- map(grains, get_stats) |> list_rbind()

# save results
results %>% write_csv("../results/meta/Checklists_by_scale.csv")

# pivot results (longer) for plotting
results <- 
  results %>% 
  pivot_longer(cols = starts_with("n"), names_to = "type", values_to="percent") %>% 
  mutate(percent = percent * 100)

# plot results
results %>% 
  ggplot(aes(x = grain, y = percent, color = type)) +
  geom_point() +
  geom_line() +
  scale_x_log10(
    breaks = grains,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    minor_breaks = NULL
  ) +
  xlab("grain size (m)") +
  ylab("percent of cells (%)") +
  labs(color = "observations")
         