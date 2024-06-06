# Get general observation statistics

library(tidyverse)
library(sf)

obs_London <- read_rds("../data/obs_London.rds") %>% st_as_sf() %>% st_transform(3857)
hbins <- read_rds("../results/R1 - London 500m 8L/hbins.rds")
estD_pts <- st_read("../results/R1 - London 500m 8L/geojson/estD_pts_q1.geojson")


# filter observations to hbin extent
obs_aoi <- st_filter(obs_London, hbins)

obs_London_sub <- obs_aoi %>% filter(protocol_type == 'Stationary' | effort_distance_km <= 1)
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(observer_id))
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(checklist_id))
obs_London_sub %>% st_drop_geometry() %>% summarise(count = n_distinct(scientific_name))

estD_pts <- st_read("../results/R1 - London 500m 8L/geojson/estD_pts_q1.geojson")

estD_hbins <- read_rds("../results/R1 - London 500m 8L/hbins_estD.rds")
estD <- estD_hbins %>% drop_na(qD_1)
q0 <- estD_hbins %>% filter(q0_spread <= 12)
q1 <- estD_hbins %>% filter(q1_spread <= 12)
q2 <- estD_hbins %>% filter(q2_spread <= 12)


# plot estD by nLists
estD_pts %>% 
  filter(nLists >= 5) %>% 
  ggplot(aes(x = nLists, y = estD)) +
  geom_point(alpha = 0.2) + scale_x_log10()

# summarize mean estD by number of completed checklists
summary <- estD_pts %>%
  group_by(nLists) %>% 
  summarise(meanD = mean(estD), n = n())

# plot meanD by nLists
summary %>% 
  filter(nLists >= 5) %>% 
  ggplot(aes(x = nLists, y = meanD)) +
  geom_point() + scale_x_log10()

# compute correlation (result = not correlated)
cor.test(estD_pts$nLists, estD_pts$estD, method = "pearson")
cor.test(summary$nLists, summary$meanD, method = "pearson")
