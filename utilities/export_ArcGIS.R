# Plots scale variance

# load packages
library(tidyverse)
library(sf)
library(lazyeval)
library(glue)
library(arcgisbinding)

# specify analysis run name
run <- "R3 - London 500m 9L"
start_size <- tail(str_split(run, " ")[[1]],2)[1]
num_levels <- tail(str_split(run, " ")[[1]],1)

# load data
sve <- st_read(file.path("../results", run, "geojson/sve.geojson"))

arc.check_product()

#arc.open('C:\Users\jddein\OneDrive - University of Tennessee\dissertation\part 1\ArcGIS\Dissertation - Part 1.gdb')

arc.write("C:\\Users\\jddein\\OneDrive - University of Tennessee\\dissertation\\part 1\\ArcGIS\\Dissertation - Part 1.gdb\\sve_test", sve)

sve_open <- arc.open("C:\\Users\\jddein\\OneDrive - University of Tennessee\\dissertation\\part 1\\ArcGIS\\Dissertation - Part 1.gdb\\sve_test")
sve_level1 <- arc.select(sve_open, where_clause = "level = '1'")

sve_sd <- arc.data2sf(sve_level1)
