# Computes estimated species diversity for specified sets (hbins) of observations

# load packages
library(AzureStor)
library(iNEXT)
library(tidyverse)
library(lazyeval)
library(sf)
library(furrr)

# set parallelization options
plan(multisession)
foptions <- furrr_options(seed = 1)

# connect to file share
akey <- "UBGPX6iwIB5Jcrds6W2MM4E7y/FWfnuGnUe/03U+u/H6f8hCWT2K2S2qZf0La0DQkwJLW7R1pq3F+AStB3is/g=="
fl_endp <- storage_endpoint("https://resstore1.file.core.windows.net", key=akey)
wd <- storage_container(fl_endp, "fshare1/dissertation/part1")

# load data
hbins <- storage_load_rds(wd, "hbins.rds")
obs <- storage_load_rds(wd, "ebd_London_hbinIDs.rds")

# specify number of levels
levels <- map(unique(hbins$level), ~ paste0("level", .x))

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
      q = c(0, 1, 2),
      datatype = "incidence_freq",
      base = "coverage",
      level = 0.95,
      nboot = 30,
      conf = 0.95
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
    split(x = ., f = eval(parse(text = paste0(".$", level)))) %>%
    keep(function(x)
      length(unique(x$sampling_event_identifier)) > 5)
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
hbin_estD %>% storage_save_rds(wd, "hbins_estD.rds")
#hbin_estD %>% st_write("../data/hbins_estD.geojson")
