# Computes estimated species diversity for specified sets (hbins) of
# observations

# load packages
library(iNEXT)
library(tidyverse)
library(lazyeval)
library(sf)
#library(furrr)
library(purrr)
library(stringr)
library(glue)

estimate_diversity <- function(dataset) {
  
  # set parallelization options
  # plan(multisession)
  # foptions <- furrr_options(seed = 1)
  
  # specify number of levels
  levels <- c("level1")
  
  # define function to compute iNEXT data info in specified hbin
  compute_binInfo <- function(binObs, id) {
    # compute raw incidence from checklists
    binInfo <- binObs %>%
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
      DataInfo(datatype = "incidence_freq") %>%
      mutate(Assemblage = id)
    return(binInfo)
  }
  
  # define function to compute species diversity in specified hbin
  compute_diversity <- function(binObs, id) {
    # compute raw incidence from checklists
    incidence_data <- binObs %>%
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
      as.incfreq()   # convert raw incidence to frequency incidence
    tryCatch({
      estD <- estimateD(
        incidence_data,
        q = c(0, 1, 2),
        datatype = "incidence_freq",
        base = "coverage",
        level = 0.95,
        nboot = 100,
        conf = 0.95
      ) %>%
        mutate(Assemblage = id)
      return(estD)
    }, error = function(e) {
      cat("ERROR (id ", id, "): ", e$message, "\n")
      return(NULL)
    })
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
  
  # define function to group observations into bins by level id
  group_observations <- function(level) {
    obs_binned_level <- dataset$obs %>%
      select(common_name, sampling_event_identifier, !!sym(level)) %>%
      st_drop_geometry() %>%
      split(x = ., f = eval(parse(text = paste0(".$", level)))) %>%
      keep(function(x) # filter out bins with only 1 checklist
        length(unique(x$sampling_event_identifier)) > 1) %>% 
      keep(function(x) # filter out bins with only 1 species
        length(unique(x$common_name)) > 1)
    }
  
  # get info on bins from data
  binInfo <- NULL
  for (level in levels) {
    cat(paste0("\nComputing ",level," data info\n"))
    obs_binned_level <- group_observations(level)
    binInfo <- obs_binned_level %>% 
      #future_map_dfr(
      map_dfr(
        ~ compute_binInfo(binObs = .x, 
                          id = eval(parse(text=paste0("unique(.x$",
                                                      level,
                                                      ")")))),
        .progress = TRUE,
        .options = foptions
      ) %>%
      bind_rows(binInfo)
  }
  
  # loop through observations for each hbin and compute estimated diversity
  estD <- NULL
  for (level in levels) {
    cat(paste0("\nComputing ",level," diversity\n"))
    obs_binned_level <- group_observations(level)
    # filter out bins with sample coverage <= 0.475 (0.95/2)
    estD <- obs_binned_level[
      names(obs_binned_level) %in% (binInfo %>% 
                                      filter(SC > 0.475))$Assemblage] %>%
      #future_map_dfr(
      map_dfr(
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
  
  # join sample coverage to hbins
  hbins_estD <- hbins_estD %>% 
    dplyr::left_join(binInfo %>% 
                       select(Assemblage, SC) %>% 
                       rename(coverage = SC), 
                     by=c("id" = "Assemblage"))
  
  # compute spread of confidence interval at 95% coverage
  hbins_estD <- hbins_estD %>% 
    mutate(
      q0_spread = qD.UCL_0 - qD.LCL_0,
      q1_spread = qD.UCL_1 - qD.LCL_1,
      q2_spread = qD.UCL_2 - qD.LCL_2
    )
  hbins_estD <- hbins_estD %>% 
    mutate(
      q0_pc = percent_rank(q0_spread),
      q1_pc = percent_rank(q1_spread),
      q2_pc = percent_rank(q2_spread)
    )
  
  return(hbins_estD)
}