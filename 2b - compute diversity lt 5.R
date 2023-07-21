# Computes estimated species diversity for specified sets (hbins) of observations

# load packages
library(iNEXT)
library(tidyverse)
library(lazyeval)
library(sf)
library(furrr)
library(stringr)
library(glue)

# specify analysis run name
run <- "R3 - London 500m 9L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# set parallelization options
plan(multisession)
foptions <- furrr_options(seed = 1)

# load data
hbins <- readRDS(file.path("../results",run,glue("hbins_{start_size}_{num_levels}.rds")))
obs <- readRDS(file.path("../results",run,"ebd_London_hbinIDs.rds"))

# specify number of levels
#levels <- map(unique(hbins$level), ~ paste0("level", .x))
levels <- c("level1")

# add ID column
hbins <- hbins %>%
  mutate(id = row_number())

# define function to compute species diversity in specified hbin
compute_diversity <- function(binObs, id) {
  # compute raw incidence from checklists
  estD <- binObs %>%
    group_by(common_name, sampling_event_identifier) %>%
    summarize(present = n(), .groups = "keep") %>%
    mutate(present = 1) %>%
    pivot_wider(
      names_from = sampling_event_identifier,
      values_from = present,
      values_fill = 0
    ) %>%
    column_to_rownames(var = "common_name") %>%
    as.data.frame() %>%
    as.incfreq() %>%   # convert raw incidence to frequency incidence
    estimateD(
      q = 2,
      datatype = "incidence_freq",
      base = "coverage",
      level = 0.95,
      nboot = 0
      #conf = 0.95
    ) %>%
    mutate(Assemblage = id)
  return(estD)
}

# define function to calculate observation statistics in hbin
compute_obs_statistics = function(binObs, id) {
  bin_summary <- tibble(
    id = id,
    nObs = nrow(binObs),
    nLists = length(unique(binObs$sampling_event_identifier)),
    nSpecies = length(unique(binObs$common_name))
  )
  return(bin_summary)
}

# compute observation statistics for every hbin
stats <- NULL
for (level in levels) {
  cat(paste0("\nComputing ",level," observation statistics\n"))
  stats <- obs %>%
    select(common_name, sampling_event_identifier, !!sym(level)) %>%
    st_drop_geometry() %>%
    split(x = ., f = eval(parse(text = paste0(".$", level)))) %>%
    map_dfr(~ compute_obs_statistics(binObs = .x, id = eval(parse(text=paste0("unique(.x$",level,")")))),
            .options = foptions) %>%
    bind_rows(stats) %>%
    arrange(id)
}

# join observation statistics to hbins
hbin_stats <- hbins %>%
  dplyr::left_join(stats, by = c("id"))

# loop through observations for each hbin and compute estimated diversity
obs_binned <- NULL
estD <- NULL
for (level in levels) {
  cat(paste0("\nComputing ",level," diversity\n"))
  obs_binned_level <- obs %>%
    select(common_name, sampling_event_identifier, !!sym(level)) %>%
    st_drop_geometry() %>%
    split(x = ., f = eval(parse(text = paste0(".$", level)))) 
    %>%
    keep(function(x)
      length(unique(x$sampling_event_identifier)) <= 5)
  obs_binned <- c(obs_binned, obs_binned_level)
  estD <- obs_binned_level %>%
    future_map_dfr(
      ~ compute_diversity(binObs = .x, id = eval(parse(text=paste0("unique(.x$",level,")")))),
      .progress = TRUE,
      .options = foptions
    ) %>%
    bind_rows(estD)
}

# join computed diversity to hbins for each Hill number q value
hbin_estD <- estD %>%
  pivot_wider(
    names_from = Order.q,
    names_glue = "{.value}_{Order.q}",
    values_from = c(qD, qD.LCL, qD.UCL)
  ) %>%
  left_join(x = hbin_stats, y = ., by=c("id" = "Assemblage"))

# save results
hbin_estD %>% saveRDS(file.path("../results",run,"hbins_estD.rds"))
#hbin_estD %>% st_write("../data/hbins_estD.geojson")
#hbin_estD %>% st_write("../data/shp/hbins_estD.shp")

# convert bin polygons to points (at centroid)
pred_xy <- hbin_estD %>% 
  filter(level==1) %>%
  st_centroid() #%>% 
#st_coordinates(.$geometry) %>% 
#as.data.frame()

# export observed point values (obs < 5) to shapefile
shp_path <- file.path("../results/",run,"shp")
if (!dir.exists(shp_path)) {
  dir.create(shp_path) 
}
pred_xy %>% drop_na(qD_2) %>% st_write(file.path(shp_path,"estD_pts.shp"))
