# Computes scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)
library(egg)
library(scales)

# load data
hbins <- readRDS("../results/R3 - London 500m 9L/hbins_500m_9L.rds")
hbins_estD <- readRDS("../results/R3 - London 500m 9L/hbins_estD.rds")

# load kriging result
preD_pts <- st_read("../results/R3 - London 500m 9L/geojson/preD_EBK_pts.geojson")
hbins_preD <- preD_pts %>% st_drop_geometry()
hbins_preD$geometry <- hbins_estD %>% filter(level==1) %>% .$geometry
hbins_preD$id_grid <- hbins_estD %>% filter(level==1) %>% .$ID
hbins_preD <- st_as_sf(hbins_preD)
hbins_preD <- hbins_preD %>% rename(id = id_grid) %>% select(-id_1)

# visualize predicted diversity
ggplot() + 
  geom_sf(data = hbins_preD, aes(fill = Predicted), color=NA) +
  scale_fill_viridis_c(option = "A")

# create table data frame with hbin IDs per level
df <- hbins_preD %>%
  select(id, Predicted) %>%
  rename(id_level1 = id, preD_level1 = Predicted)

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
  filter(!is.na(preD_level1))

# TEMP: remove all 'value' columns for all levels except level1
df <- df %>% st_drop_geometry() %>% rename(X_level1 = preD_level1) %>% select(-starts_with("qD")) %>% rename(qD_2_level1 = X_level1)

# !!! TODO: remove hard coded level name
# add column for final level (global value)
df <- df %>%
  mutate(id_level10 = max(hbins_estD$id)+1,
         qD_2_level10 = mean(qD_2_level1))

# define function to compute bin value (mean of bins from level below) for a specified level
compute_level_mean <- function(leveln) {
  df <<- df %>%
  group_by(!!sym(glue("id_level{leveln}"))) %>%
    summarise("qD_2_level{leveln}" := mean(!!sym(glue("qD_2_level{leveln-1}")))) %>%
    left_join(df, .)
}

# compute bin values for all levels except level 1
r <- c(2:9) %>%
  map(compute_level_mean)


# visualize bins at all levels
# df_geom <- data.frame(df)
# df_geom$geometry <- hbins_preD$geometry
# df_geom <- st_as_sf(df_geom)
# 
# p1 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level1), color = NA) +
#   scale_fill_viridis_c(option = "A")
# p2 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level2), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level2)/100, end=max(df$qD_2_level1)/100)
# p3 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level3), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# p4 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level4), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# p5 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level5), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# 
# p6 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level6), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# p7 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level7), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# p8 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level8), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# p9 <- ggplot() + 
#   geom_sf(data = df_geom, aes(fill = qD_2_level9), color = NA) +
#   scale_fill_viridis_c(option = "A", begin=min(df$qD_2_level1)/100, end=max(df$qD_2_level1)/100)
# 
# ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3, nrow=3)
# p2

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

# define function to compute scale variance elements for leveln
compute_sv_elements <- function(leveln) {
  return(
    df %>% 
      group_by(!!sym(glue("id_level{leveln}"))) %>%
      summarise(col1 = mean(!!sym(glue("qD_2_level{leveln}"))), col2 = mean(!!sym(glue("qD_2_level{leveln+1}")))) %>%
      mutate(sv = ((col2-col1)^2)) %>% 
      mutate(sv_n = sv/compute_degf(leveln)) %>% 
      rename(id = !!sym(glue("id_level{leveln}"))) %>% 
      left_join(hbins, by = c("id" = "ID"), keep = FALSE) %>% 
      st_as_sf()
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
sv$scale_variance[9] <- 0.01

# compute scale variance elements for all levels
sve <- levels %>% map(compute_sv_elements)

sve_bound <- sve %>% bind_rows(.id="id") %>% 
  mutate(sv_log = log10(sv))


sve_bound %>% st_write(glue("../results/R3 - London 500m 9L/geojson/sve.geojson"))


ggplot() + 
  geom_sf(data = sve_bound %>% filter(level < 9), aes(fill = sv), color=NA) +
  #scale_fill_viridis_c(option = "D", limits=c(0,250), oob='squish') + 
  scale_fill_gradientn(colors=c('white', 'green'), limits=c(0,200), oob=squish) + 
  facet_wrap(facets = vars(level), nrow=3, ncol=3)

sve_bound %>% filter(level < 9) %>% 
ggplot(aes(x = sv_log, fill = level)) +
  geom_histogram(stat='density') + 
  facet_wrap(facets=vars(level), nrow=3, ncol=3)

# define function to export scale variance elements (sve) for each level
write_sve <- function(level) {
  st_write(sve[[level]], glue("../results/R3 - London 500m 9L/geojson/sv{level}.geojson"))
}
# export scale variance elements for each level
levels %>% map(write_sve)

# visualize predicted diversity
ggplot() + 
  geom_sf(data = sve[[4]], aes(fill = sv), color=NA) +
  scale_fill_viridis_c(option = "A")

# !!! TODO: remove hard coded level name
# compute total sum of squares
tss <- sum((df$qD_2_level1 - df$qD_2_level10)^2)

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
         scale = compute_scales(500, 9),
         sv_cumulative = cumsum(scale_variance),
         degf = as.numeric(degf),
         sum_squares = as.numeric(ss)
  ) #%>% 
  #write_csv("../results/R3 - London 500m 9L/sv_q2_EBK.csv")

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

#ggsave("../results/R3 - London 500m 9L/scale_variance_q2 EBK.png")
