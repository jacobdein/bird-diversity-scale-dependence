# Utility functions to work with arcgis data formats

library(arcgisbinding)

# get arcgis license
arc.check_product()

# define function to read data from a geodatabase as an sf object
read_arcgis <- function(name) {
  return(arc.data2sf(arc.select(arc.open(name))))
}

# define function to output data to a geodatabase
write_arcgis <- function(name, data) {
  arc.write(name, data, overwrite=TRUE)
}