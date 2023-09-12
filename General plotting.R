# General plotting


# hbins
# plot resulting features
# aoi
plot(st_read(run_aoi)$geometry)
# highest level
plot(filter(hbins, level == run_levels)$geometry, add=TRUE)