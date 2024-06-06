# Plots scale variance

# load packages
library(tidyverse)
library(sf)
library(glue)
library(scales)
library(sfdep)
library(terra)
library(tidyterra)
library(ggpubr)

aoi <- st_read(file.path("../data/GreaterLondon.geojson"))

sv_SET <- read_rds("../results/R1 - London 500m 8L/sv_OBS_q1.rds")
sve <- map_df(sv_SET$sve, function(l) {return(l)})

sv_sum <- sve %>% 
  st_drop_geometry() %>%  
  group_by(level) %>% 
  summarize(sv_sum = sum(sv)) %>% 
  mutate(degf = as.numeric(sv_SET$degf)) %>% 
  mutate(svc = sv_sum/degf) %>% 
  mutate(svc_norm = (svc/sum(svc)*100))

scales <- c("500", "1000", "2000", "4000", "8000", "16000", "32000", "64000")

p <- list()
for(i in 1:8) {
  p[[i]] <- sv_SET$sve[[i]] %>% ggplot() +
    labs(title=glue("{scales[[i]]}m - {sprintf('%.1f', sv_sum$svc_norm[i])}%")) +
    geom_sf(aes(fill = sv), color=NA) +
    geom_sf(data = aoi, fill=NA, linewidth=1) +
    scale_fill_gradientn(name = "squared deviation",
      colors=c('#f7f7f7','#cccccc','#969696','#525252'), 
      limits=c(0,400), oob=squish, 
      breaks = c(0, 100, 200, 300, 400),
      labels = c("0", "100", "200", "300", "400+")) +
    theme_void()
}

ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], 
          p[[5]], p[[6]], p[[7]], p[[8]], 
          ncol=4, nrow=2, common.legend = TRUE, legend="bottom")
