# Computes scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)

# specify analysis run name
run <- "R4 - London 1000m 7L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
hbins_estD <- readRDS(file.path("../results", run, "hbins_estD.rds"))

# create table data frame with hbin IDs per level
df <- hbins_estD %>%
  dplyr::filter(level == 1) %>%
  dplyr::filter(nLists >= 5) %>%  # remove hbins with less than 5 complete checklists
  select(id, qD_2) %>%
  rename(id_level1 = id, qD_2_level1 = qD_2)

# define function to join parent bins for a specified bin level
join_parent_bins <- function(leveln) {
  return(
    st_join(
      df,
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
df <- levels[-1] %>%
  map_dfc(join_parent_bins) %>%
  bind_cols(df, .)

# remove level 1 bins with no value
df <- df %>%
  filter(!is.na(qD_2_level1))

# TEMP: remove all 'value' columns for all levels except level1
df <- df %>% st_drop_geometry() %>% rename(X_level1 = qD_2_level1) %>% select(-starts_with("qD")) %>% rename(qD_2_level1 = X_level1)

# !!! TODO: remove hard coded level name
# add column for final level (global value)
df <- df %>%
  mutate(id_level8 = max(hbins_estD$id)+1,
         qD_2_level8 = mean(qD_2_level1))

# define function to compute bin value (mean of bins from level below) for a specified level
compute_level_mean <- function(leveln) {
  df <<- df %>%
  group_by(!!sym(glue("id_level{leveln}"))) %>%
    summarise("qD_2_level{leveln}" := mean(!!sym(glue("qD_2_level{leveln-1}")))) %>%
    left_join(df, .)
}

# compute bin values for all levels except level 1
r <- levels[-1] %>%
  map(compute_level_mean)

# define function to compute squared differences between leveln and leveln+1
compute_squares <- function(leveln) {
  return(
    df %>%
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
    df %>%
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
    df %>% 
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
#sv$scale_variance[7] <- 1

# !!! TODO: remove hard coded level name
# compute total sum of squares
tss <- sum((df$qD_2_level1 - df$qD_2_level8)^2)

# compute total degrees of freedom
tdegf <- df %>% nrow()-1

compute_scales <- function(start, nlevels) {
  scales <- c(1:nlevels) %>% map(~start*2^(.x-1))
  return(as.numeric(scales))
}

# transform sv to %sv
sv_p <- sv %>% 
  mutate(scale_variance = scale_variance/sum(sv$scale_variance),
# !!! TODO: remove hard coded values
         scale = compute_scales(1000, 7),
         sv_cumulative = cumsum(scale_variance),
         degf = as.numeric(degf),
         sum_squares = as.numeric(ss)
  ) %>% 
  write_csv(file.path("../results", run, "sv_q2.csv"))

# plot results
sv_p %>%
ggplot(aes(x=scale, y=sv_cumulative, label = scale)) +
  scale_x_continuous(trans='log10') +
  #geom_col(width = 0.001, fill='black') +
  geom_point(size=3) + 
  geom_text(nudge_y = -0.05) + 
  geom_line() +
  xlab('scale (m)') +
  ylab('Cumulative variance (%)') + 
  ggtitle('Scale variance of bird diversity (Hill number q = 2)')

ggsave(file.path("../results", run, "scale_variance_q2.png"))
