# Joins hbin IDs to ebd observations

# load packages
library(tidyverse)
library(stringr)
library(glue)
library(sf)

# specify analysis run name
run <- "R5 - London 750m 7L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
hbins <- st_read(file.path("../results",run,glue("hbins_{start_size}_{num_levels}.geojson")))
# ebd <- read.csv("../data/ebd_London.csv")
# obs <- st_as_sf(ebd, coords=c("longitude", "latitude"), crs=4326) %>% st_transform(crs=3857)
obs <- readRDS("../data/obs_London.rds")

# add ID column
hbins <- hbins %>%
  mutate(ID = row_number())

# filter observations to hbin extent
obs_aoi <- st_filter(obs, hbins)

# join hbin ID to every observation for each hbin level
# TODO: parallelize with furrr and add progress indicator
levels = unique(hbins$level)
for (l in levels) {
  obs_aoi <- obs_aoi %>% 
    st_join(filter(hbins, level == l) %>% 
              select(-level), join=st_within) %>% 
    rename_with(.fn = ~paste0('level',l), .cols=c("ID"))
}

# save resulting features
#obs_aoi %>% st_write("../data/ebd_London_hbinIDs.geojson")
obs_aoi %>% saveRDS(file.path("../results",run,"ebd_London_hbinIDs.rds"))
hbins %>% saveRDS(file.path("../results",run,glue("hbins_{start_size}_{num_levels}.rds")))
