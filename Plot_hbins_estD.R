library(tidyverse)
library(sf)
library(ggpubr)
library(scales)
library(glue)
library(RColorBrewer)

aoi <- st_read(file.path("../data/GreaterLondon.geojson"))

OBS_estD_values <- st_read("../results/R22 - London 500m 8L/geojson/OBS_estD_values.geojson")
OK_estD_values <- st_read("../results/R22 - London 500m 8L/geojson/OK_estD_values.geojson")
EBK_estD_values <- st_read("../results/R22 - London 500m 8L/geojson/EBK_estD_values.geojson")

datasets <- list(OBS_estD_values, OK_estD_values, EBK_estD_values)
titles <- c("Observed", "Ordinary Kriging", "Empirical Bayesian Kriging")

scales <- c("500", "1000", "2000", "4000", "8000", "16000", "32000", "64000")

map_labeller <- function(level) {
  return(scales[level])
}

# Get the "BrBG" colormap with 6 colors
colors <- brewer.pal(6, "BrBG")

# Function to assign colors based on standard deviation from mean
assign_color <- function(x, mean, sd) {
  sd_diff <- (x - mean) / sd
  if (sd_diff < -2) return(colors[1])
  else if (sd_diff >= -2 & sd_diff < -1) return(colors[2])
  else if (sd_diff >= -1 & sd_diff < 0) return(colors[3])
  else if (sd_diff >= 0 & sd_diff < 1) return(colors[4])
  else if (sd_diff >= 1 & sd_diff < 2) return(colors[5])
  else return(colors[6])
}

hists <- list()

for (i in 1:length(datasets)) {
  mean <- datasets[[i]]$est %>% mean()
  std <- datasets[[i]]$est %>% sd()

  # Add a column for colors
  datasets[[i]] <- datasets[[i]] %>% mutate(color = sapply(est, assign_color, mean = mean, sd = std))
  
  hists[[i]] <- datasets[[i]] %>% 
    ggplot(aes(x = est, fill = color)) +
    geom_histogram(binwidth = 1) +
    scale_fill_identity(guide = "legend") +
    theme_minimal() +
    xlim(0,100) +
    labs(#title = glue("{titles[i]}"),
         x = "estimated diversity (q2)",
         y = "frequency")
}
ggarrange(hists[[1]], hists[[2]], hists[[3]], nrow=1, ncol=3, labels="auto", common.legend = TRUE, legend = "bottom")

maps <- list()

for (i in 1:length(datasets)) {
  maps[[i]] <- datasets[[i]] %>% 
    ggplot(aes(fill = color)) +
    geom_sf(color=NA) +
    geom_sf(data = aoi, fill=NA, linewidth=1) +
    scale_fill_identity() +
    #scale_fill_gradientn(colors=c('#a6611a','#dfc27d','#f5f5f5','#80cdc1', '#018571'), na.value = NA, limits=c(0,100), oob=squish) +
    theme_void() +
    facet_wrap(vars(level), nrow=2, ncol=4)
}

ggarrange(maps[[1]], maps[[2]], maps[[3]], col=1, nrow=3, common.legend = TRUE, legend="bottom")



datasets[[1]]$est %>% mean()
datasets[[1]]$est %>% sd()

datasets[[2]]$est %>% mean()
datasets[[2]]$est %>% sd()

datasets[[3]]$est %>% mean()
datasets[[3]]$est %>% sd()
