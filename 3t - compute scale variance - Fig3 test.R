# Computes scale variance VERIFICATION OF CODE WITH DATA FROM FIG3 in Moellering and Tobler 1972

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)

# load test data
df <- read_csv("../data/Fig3/ValueHierarchy.csv")

qD_2_2 <- df %>%
  group_by(id_level2) %>%
  summarise(qD_2_level2 = mean(qD_2_level1))

df <- df %>% left_join(qD_2_2, by="id_level2")

qD_2_3 <- df %>%
  group_by(id_level3) %>%
  summarise(qD_2_level3 = mean(qD_2_level2))

df <- df %>% left_join(qD_2_3, by="id_level3")

qD_2_4 <- df %>%
  group_by(id_level4) %>%
  summarise(qD_2_level4 = mean(qD_2_level3))

df <- df %>% left_join(qD_2_4, by="id_level4")

df <- df %>%
  mutate(qD_2_level5 = mean(df$qD_2_level1))


# specify list of levels
levels <- c(1,2,3,4)

# define function to compute squared differences between leveln and leveln+1
compute_squares <- function(leveln) {
  return(
    df %>% st_drop_geometry() %>%
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
    df %>% 
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
  map(compute_sv)

# compute total sum of squares
tss <- sum((df$qD_2_level1 - df$qD_2_level5)^2)

# compute total degrees of freedom
tdegf <- df %>% nrow()-1
