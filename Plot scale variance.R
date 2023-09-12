# Plots scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)
library(scales)
library(grid)

source("./utilities/shift_legend.R")

# specify analysis run name
run <- "R3 - London 500m 9L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
sve <- st_read(file.path("../results", run, "geojson/sve.geojson"))

# plot data
p <- ggplot() + 
  geom_sf(data = sve %>% filter(level < 9), aes(fill = sv), color=NA) +
  scale_fill_gradientn(colors=c('white', 'green'), limits=c(0,200), oob=squish) + 
  facet_wrap(facets = vars(level), nrow=3, ncol=3) +
  theme_minimal() +
  theme(
    panel.background = element_rect(colour = "lightgray", linewidth = 1)
    #panel.border = element_rect(colour = "lightgray", linewidth = 1)
  )

grid.draw(shift_legend(p))



# plot sv results
# sv_p %>%
#   ggplot(aes(x=scale, y=sv_cumulative, label = scale)) +
#   scale_x_continuous(trans='log10') +
#   #geom_col(width = 0.001, fill='black') +
#   geom_point(size=3) + 
#   geom_text(nudge_y = -0.05) + 
#   geom_line() +
#   xlab('scale (m)') +
#   ylab('Cumulative variance (%)') + 
#   ggtitle('Scale variance of bird diversity (Hill number q = 2)')


# plot sve
# ggplot() + 
#   geom_sf(data = sve_bound %>% filter(level < 9), aes(fill = sv), color=NA) +
#   #scale_fill_viridis_c(option = "D", limits=c(0,250), oob='squish') + 
#   scale_fill_gradientn(colors=c('white', 'green'), limits=c(0,200), oob=squish) + 
#   facet_wrap(facets = vars(level), nrow=3, ncol=3)

# plot sve hists
# sve_bound %>% filter(level < 9) %>% 
#   ggplot(aes(x = sv_log, fill = level)) +
#   geom_histogram(stat='density') + 
#   facet_wrap(facets=vars(level), nrow=3, ncol=3)