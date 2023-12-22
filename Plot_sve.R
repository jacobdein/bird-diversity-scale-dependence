# Plots scale variance

# load packages
library(tidyverse)
library(sf)
#library(lazyeval)
library(glue)
library(scales)
#library(grid)
library(sfdep)
library(terra)
library(tidyterra)

library(ggpubr)

#library(gridExtra)

aoi <- st_read(file.path("../data/GreaterLondon.geojson"))

sv_SET <- read_rds("../results/R2 - London 500m 8L/sv_EBK.rds")
sve <- map_df(sv_SET$sve, function(l) {return(l)})

# lisa <- sve %>% filter(level == 4) %>% 
#   mutate(
#     nb = st_contiguity(geometry),
#     wt = st_weights(nb)
#   ) %>% 
#   mutate(moran = local_moran(sv, nb, wt)) %>% 
#   tidyr::unnest(moran) %>% 
#   mutate(pysal = ifelse(p_folded_sim <= 0.05, as.character(pysal), NA))


compute_local_moran <- function(l) {
  sve %>% filter(level == l) %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb)
  ) %>% 
    mutate(moran = local_moran(sv, nb, wt)) %>% 
    tidyr::unnest(moran) %>% 
    mutate(pysal = ifelse(p_folded_sim <= 0.05, as.character(pysal), NA)) %>% 
    dplyr::rename(scale_variance = sv, cluster = pysal)
}

lisas <- map(sve$level %>% unique(), compute_local_moran)



cluster_colors <- c("#F0B8B1", "#B7D9E8", "#1B53E0", "#E01B1B")
names(cluster_colors) <- c("High-Low", "Low-High", "Low-Low", "High-High")

scales <- c("500", "1000", "2000", "4000", "8000", "16000", "32000", "64000")


#' p <- lisas %>%
#'   vect() %>% 
#'   terra::aggregate('pysal', dissolve=TRUE) |> 
#'   ggplot() +
#'   geom_sf(data = sve %>% filter(level == 4), aes(fill = sv), color=NA) +
#'   geom_sf(data = aoi, fill=NA, linewidth=1) +
#'   #'#edf8fb'
#'   scale_fill_gradientn(colors=c('white','#b2e2e2','#66c2a4','#238b45'), limits=c(0,100), oob=squish) +
#'   geom_spatvector(aes(color=pysal), fill=NA, linewidth=1) +
#'   theme_void() +
#'   scale_color_manual(values = cluster_colors, na.value = NA)
#' 
#' p

dissolve <- function(i) {
  lisa_vec <- lisas[[i]] %>% 
    vect()
  if (lisa_vec %>% filter(!is.na(cluster)) %>% nrow() > 0) {
    lisa_vec <- lisa_vec %>% 
      terra::aggregate('cluster', dissolve=TRUE) %>% 
      terra::buffer(-50*i^1.2, capstyle="round", joinstyle="round")
  }
  return(lisa_vec)
}

p <- list()
for(i in 1:8) {
  p[[i]] <- lisas[[i]] %>% 
    ggplot() +
    labs(title=glue("{scales[[i]]}m")) +
    geom_sf(aes(fill = scale_variance), color=NA) +
    geom_sf(data = aoi, fill=NA, linewidth=1) +
    scale_fill_gradientn(colors=c('#f7f7f7','#cccccc','#969696','#525252'), limits=c(0,100), oob=squish) +
    #scale_fill_gradientn(colors=c('white','#b2e2e2','#66c2a4','#238b45'), limits=c(0,100), oob=squish) +
    geom_spatvector(data=dissolve(i), aes(color=cluster), fill=NA, linewidth=(0.05)*(i^1.8)) +
    theme_void() +
    #theme(legend.position="none") +
    scale_color_manual(values = cluster_colors, na.value = NA)
}
#do.call(grid.arrange,p)
ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], 
          p[[5]], p[[6]], p[[7]], p[[8]], 
          ncol=4, nrow=2, common.legend = TRUE, legend="bottom")


#' p <- ggplot() + 
#'   geom_sf(data = lisas, aes(fill = sv), color=NA) +
#'   geom_sf(data = aoi, fill=NA) +
#'   #'#edf8fb'
#'   scale_fill_gradientn(colors=c('white','#b2e2e2','#66c2a4','#238b45'), limits=c(0,100), oob=squish) + 
#'   facet_wrap(facets = vars(level), nrow=3, ncol=3) +
#'   theme_minimal() +
#'   theme(
#'     panel.background = element_rect(colour = "lightgray", linewidth = 1)
#'     #panel.border = element_rect(colour = "lightgray", linewidth = 1)
#'   )
#' 
#' grid.draw(shift_legend(p))