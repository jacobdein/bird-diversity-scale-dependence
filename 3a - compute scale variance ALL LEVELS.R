# Computes scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)

# load data
hbins_estD <- readRDS("../data/hbins_estD.rds")

# create table data frame with hbin IDs per level
hbin_levels <- hbins_estD %>%
  dplyr::filter(level == 1) %>%
  select(id, qD_2) %>%
  rename(id_level1 = id, qD_2_level1 = qD_2)

# define function to join parent bins for a specified bin level
join_parent_bins <- function(leveln) {
  return(
    st_join(
      hbin_levels,
      filter(hbins_estD, level == leveln) %>% select(id, qD_2),
      join = st_within,
      left = TRUE,
      suffix = c("_X", paste0("_level", leveln))
    ) %>%
      st_drop_geometry() %>%
      select(id,qD_2) %>%
      rename_with(~paste0(.x, "_level", leveln))
  )
}

# specify list of levels
levels <- hbins_estD$level %>% unique() %>% as.numeric()

# join parent bins for specified levels (all except level 1)
hbin_levels <- levels[-1] %>%
  map_dfc(join_parent_bins) %>%
  bind_cols(hbin_levels, .)

# add column for final level (global value)
hbin_levels <- hbin_levels %>%
  mutate(id_level6 = max(hbins_estD$id)+1,
         qD_2_level6 = mean(qD_2_level1, na.rm=TRUE))

# remove level 1 bins with no value
hbin_levels <- hbin_levels %>%
  filter(!is.na(qD_2_level1))

# define function to compute squared differences between leveln and leveln+1
compute_squares <- function(leveln) {
  return(
    hbin_levels %>% st_drop_geometry() %>%
      transmute("sq_level{leveln}" := (!!sym(glue("qD_2_level{leveln+1}")) - !!sym(glue("qD_2_level{leveln}")))^2)
  )
}

# compute sum of squares for all levels
ss <- levels %>%
  map_dfc(compute_squares) %>%
  summarise(across(everything(), ~ sum(.)))

# define function to compute degrees of freedom for leveln
compute_degf <- function(leveln) {
  return(
    hbin_levels %>%
      st_drop_geometry() %>%
      group_by(!!sym(glue("id_level{leveln+1}"))) %>%
      summarize(count = n_distinct(!!sym(glue("id_level{leveln}")))) %>%
      mutate(degf_ = count - 1) %>%
      pull(degf_) %>%
      sum
  )
}

# define function to compute degrees of freedom for all levels
degf <- levels %>%
  map(compute_degf)

# define function to compute scale variance component for leveln
compute_sv <- function(leveln) {
  return(
    hbin_levels %>% 
      st_drop_geometry() %>% 
      group_by(!!sym(glue("id_level{leveln}"))) %>%
      summarise(col1 = mean(!!sym(glue("qD_2_level{leveln}"))), col2 = mean(!!sym(glue("qD_2_level{leveln+1}")))) %>%
      mutate(sv = ((col2-col1)^2)) %>% 
      pull(sv) %>% 
      sum/compute_degf(leveln)
  )
}

# compute scale variance component for all levels
sv <- levels %>%
  map(compute_sv) %>%
  as.numeric() %>%
  tibble(
    scale_variance = .,
    level = levels
  )

# compute total sum of squares
tss <- sum((hbin_levels$qD_2_level1 - hbin_levels$qD_2_level6)^2)

# compute total degrees of freedom
tdegf <- hbin_levels %>% nrow()-1


# plot results
sv %>%
ggplot(aes(x=level, y=scale_variance )) +
  geom_line() +
  geom_point()
