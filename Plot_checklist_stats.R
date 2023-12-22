# Plots nLists (checklists) distribution and variation in the difference 
# between estimated diversity and the EBK prediction

library(tidyverse)
library(sf)
library(scales)
library(ggpubr)
library(RColorBrewer)


# OBS_estD_values <-
#   st_read("../results/R22 - London 500m 8L/geojson/OBS_estD_values.geojson") %>%
#   filter(level == 1)
aoi <- st_read(file.path("../data/GreaterLondon.geojson"))
hbins <- read_rds("../results/R22 - London 500m 8L/hbins.rds") %>% 
  filter(level == 1)
OK_estD_values <- 
  st_read("../results/R22 - London 500m 8L/geojson/OK_estD_values.geojson") %>% 
  filter(level == 1)
EBK_estD_values <- 
  st_read("../results/R22 - London 500m 8L/geojson/EBK_estD_values.geojson") %>% 
  filter(level == 1)

estD_pts <- st_read("../results/R22 - London 500m 8L/geojson/estD_pts.geojson")

# join nLists to OBS polygons
checklists <- hbins %>% 
  st_join(estD_pts %>% select(nLists))

# point estD predictions from OK and EBK results
estD_pts <- estD_pts %>% 
  st_join(OK_estD_values %>% select(est) %>% rename(estD_OK = est)) %>% 
  st_join(EBK_estD_values %>% select(est) %>% rename(estD_EBK = est))

# rename kriging prediction columns
estD_pts <- estD_pts %>% 
  mutate(diff_OK = estD - estD_OK) %>% 
  mutate(diff_EBK = estD - estD_EBK) 


# Get the "BrBG" colormap with 6 colors
colors <- brewer.pal(6, "YlGnBu")
#colors <- setNames(colors, c("1", "2", "3", "4", "5", "6"))

# Function to assign colors based on standard deviation from mean
assign_color <- function(x, quants) {
  if (!is.na(x)) {
    if (x <= 4) return("#cccccc")
    else if (x > 4 & x < quants[1]) return(colors[1])
    else if (x >= quants[1] & x < quants[2]) return(colors[2])
    else if (x >= quants[2] & x < quants[3]) return(colors[3])
    else if (x >= quants[3] & x < quants[4]) return(colors[4])
    else if (x >= quants[4] & x < quants[5]) return(colors[5])
    else return(colors[6])
  }
  else {
    return(NA)
  }
}
quants <- quantile(checklists %>% filter(nLists > 4) %>% .$nLists, c(0.167,0.33,0.5,0.667,0.837), na.rm=TRUE)
checklists <- checklists %>% 
  # drop_na() %>% 
  mutate(color = sapply(nLists, assign_color, quants = quants))

# map of number of completed checklists aggregated into grid cells
p1 <- checklists %>% 
  ggplot(aes(fill = color)) +
  geom_sf(color=NA) +
  geom_sf(data = aoi, fill=NA, linewidth=1) +
  #scale_fill_identity() +
  scale_fill_identity() +
  # scale_fill_gradientn(colors=c('#a6611a','#dfc27d','#f5f5f5','#80cdc1', '#018571'), na.value = NA, limits=c(0,100), oob=squish) +
  theme_void()

# distribution of aggregated counts of completed checklists
p2 <- checklists %>% 
  filter(nLists > 4) %>% 
  ggplot(aes(x = nLists, fill = color)) +
  geom_histogram(binwidth = 1) +
  scale_fill_identity() +
  xlim(0, 100) +
  labs(x = "number of checklists", y = "count of cells")
#ylim(0, 1000)

# difference in estimates by number of completed checklists
p3 <- estD_pts %>% 
  filter(nLists <= 10) %>% 
  ggplot(aes(as.factor(nLists), diff_EBK)) +
    geom_violin() +
  ylim(0, 100) +
  labs(x = "number of checklists", y = "difference in diveristy estimates")

ggarrange(p1, p2, p3, nrow = 1, ncol = 3, labels = "auto")
