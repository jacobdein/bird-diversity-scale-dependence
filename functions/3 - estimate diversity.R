# Computes estimated species diversity for specified sets (hbins) of
# observations

# load packages
library(iNEXT)
library(tidyverse)
library(lazyeval)
library(sf)
library(furrr)
library(stringr)
library(glue)

estimate_diversity <- function(dataset) {
  
  # set parallelization options
  plan(multisession)
  foptions <- furrr_options(seed = 1)
  
  # specify number of levels
  levels <- c("level1")
  
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
    stats <- dataset$obs %>%
      select(common_name, sampling_event_identifier, !!sym(level)) %>%
      st_drop_geometry() %>%
      split(x = ., f = eval(parse(text = paste0(".$", level)))) %>%
      map_dfr(~ compute_obs_statistics(binObs = .x, 
                                       id = eval(parse(text=paste0("unique(.x$",
                                                                   level,
                                                                   ")")))),
              .options = foptions) %>%
      bind_rows(stats) %>%
      arrange(id)
  }
  
  # join observation statistics to hbins
  hbins_stats <- dataset$hbins %>%
    dplyr::left_join(stats, by = c("id"))
  
  # loop through observations for each hbin and compute estimated diversity
  obs_binned <- NULL
  estD <- NULL
  for (level in levels) {
    cat(paste0("\nComputing ",level," diversity\n"))
    obs_binned_level <- dataset$obs %>%
      select(common_name, sampling_event_identifier, !!sym(level)) %>%
      st_drop_geometry() %>%
      split(x = ., f = eval(parse(text = paste0(".$", level)))) 
    # %>%
    # keep(function(x)
    #   length(unique(x$sampling_event_identifier)) <= 5)
    obs_binned <- c(obs_binned, obs_binned_level)
    estD <- obs_binned_level %>%
      future_map_dfr(
        ~ compute_diversity(binObs = .x, 
                            id = eval(parse(text=paste0("unique(.x$",
                                                        level,
                                                        ")")))),
        .progress = TRUE,
        .options = foptions
      ) %>%
      bind_rows(estD)
  }
  
  # join computed diversity to hbins for each Hill number q value
  hbins_estD <- estD %>%
    pivot_wider(
      names_from = Order.q,
      names_glue = "{.value}_{Order.q}",
      values_from = c(qD, qD.LCL, qD.UCL)
    ) %>%
    left_join(x = hbins_stats, y = ., by=c("id" = "Assemblage"))
  
  return(hbins_estD)
}

