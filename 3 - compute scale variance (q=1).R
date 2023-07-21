# Computes scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)

# load data
hbins_estD <- readRDS("../results/R1 - London 1000m 5L/hbins_estD.rds")

# create table data frame with hbin IDs per level
df <- hbins_estD %>%
  dplyr::filter(level == 1) %>%
  select(id, qD_1) %>%
  rename(id_level1 = id, qD_1_level1 = qD_1)

# define function to join parent bins for a specified bin level
join_parent_bins <- function(leveln) {
  return(
    st_join(
      df,
      filter(hbins_estD, level == leveln) %>% select(id, qD_1),
      join = st_within,
      left = TRUE,
      suffix = c("_X", paste0("_level", leveln))
    ) %>%
      st_drop_geometry() %>%
      select(id,qD_1) %>%
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
  filter(!is.na(qD_1_level1))

# TEMP: remove all 'value' columns for all levels except level1
df <- df %>% st_drop_geometry() %>% rename(X_level1 = qD_1_level1) %>% select(-starts_with("qD")) %>% rename(qD_1_level1 = X_level1)

# add column for final level (global value)
df <- df %>%
  mutate(id_level6 = max(hbins_estD$id)+1,
         qD_1_level6 = mean(qD_1_level1))

# define function to compute bin value (mean of bins from level below) for a specified level
compute_level_mean <- function(leveln) {
  df <<- df %>%
  group_by(!!sym(glue("id_level{leveln}"))) %>%
    summarise("qD_1_level{leveln}" := mean(!!sym(glue("qD_1_level{leveln-1}")))) %>%
    left_join(df, .)
}

# compute bin values for all levels except level 1
r <- c(2:5) %>%
  map(compute_level_mean)

# define function to compute squared differences between leveln and leveln+1
compute_squares <- function(leveln) {
  return(
    df %>%
      transmute("sq_level{leveln}" := (!!sym(glue("qD_1_level{leveln+1}")) - !!sym(glue("qD_1_level{leveln}")))^2)
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
      summarise(col1 = mean(!!sym(glue("qD_1_level{leveln}"))), col2 = mean(!!sym(glue("qD_1_level{leveln+1}")))) %>%
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
tss <- sum((df$qD_1_level1 - df$qD_1_level6)^2)

# compute total degrees of freedom
tdegf <- df %>% nrow()-1

compute_scales <- function(start, nlevels) {
  scales <- c(1:nlevels) %>% map(~start*2^(.x-1))
  return(as.numeric(scales))
}

# transform sv to %sv
sv_p <- sv %>% 
  mutate(scale_variance = scale_variance/sum(sv$scale_variance)) %>%
  mutate(scale = compute_scales(1000, 5)) %>%
  mutate(sv_cumulative = cumsum(scale_variance)) %>%
  write_csv("../results/R1 - London 1000m 5L/sv_q1.csv")

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
  ggtitle('Scale variance of bird diversity (Hill number q = 1)')

ggsave("../results/R1 - London 1000m 5L/scale_variance_q1.png")
