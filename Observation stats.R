library(tidyverse)

obs_London <- read_rds("../data/obs_London.rds") %>% st_as_sf() %>% st_transform(3857)

obs_London_sub <- obs_London %>% filter(protocol_type == 'Stationary' | effort_distance_km <= 1)
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(observer_id))
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(checklist_id))
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(scientific_name))

