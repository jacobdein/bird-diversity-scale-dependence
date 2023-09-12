# Run Analysis
# Jacob Dein
# September 2023


library(glue)
library(tidyverse)
library(arcgisbinding)


### specify run parameters ###
# specify an identifier to id the results from this set of parameters
run_id <- "R99"
# specify a name for the area of interest (aoi)
run_location <- "London"
# specify path to aoi geometry
run_aoi <- "../data/GreaterLondon.geojson"
# specify size in meters of largest scale
run_start_scale_m <- 750
# specify the number of levels (scales)
run_levels <- 7
# specify a full name for this set of parameters
run <- glue("{run_id} - {run_location} {run_start_scale_m}m {run_levels}L")
# specify the input observations
run_obs <- "../data/obs_London.rds"
# specify the output directory to save the results
output_dir <- file.path("../results", run)

# specify if arcgis functions are available and should be used
# and conditionally set required parameters and load utility functions
arcgis <- TRUE
if(arcgis) {
  source("./utilities/arcgis.R")
  gdb <- file.path("C:\\Users\\jddein\\OneDrive - University of Tennessee",
                   "dissertation\\part 1\\ArcGIS\\Dissertation - Part 1.gdb")
  conda_path <- "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\Scripts\\conda"
  dir.create(file.path(output_dir, "arcgis"))
}

# Remove existing output directory if it exists and then create new 
# output directory 
if(dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
}
dir.create(output_dir, recursive = TRUE)



### 1 - Create hierarchical bins (hbins) ###
# Generate hierarchical bins centered around an AOI starting at 
# a specified scale, which doubles in size for the specified 
# number of levels.
source("functions/1 - create hbins.R")

hbins <- create_hbins(aoi=run_aoi, 
                      max_size=(2^(run_levels-1)*run_start_scale_m), 
                      nlevels=run_levels) %>% 
         st_as_sf() # convert hbins to sf object

# save resulting features
st_write(hbins, 
         file.path(output_dir, 
                   glue("hbins_{run_start_scale_m}m_{run_levels}L.geojson")))
if(arcgis) {
  write_arcgis(file.path(gdb, run_id, glue("{run_id}_hbins")), hbins)
}



### 2 - Join eBird observations to hbins by IDs ###
source("./functions/2 - join hbin IDs.R")

# load eBird data
obs <- readRDS(run_obs)

# join hbins
dataset <- join_hbins(hbins, obs)

# save resulting dataset
dataset$obs %>% saveRDS(file.path("../results",
                        run, glue("ebd_hbinIDs.rds")))
dataset$hbins %>% saveRDS(file.path("../results",
                   run, glue("hbins.rds")))



### 3 - Estimate diversity ###
source("functions/3 - estimate diversity.R")

# estimate species diversity
q <- 2
hbins_estD <- estimate_diversity(dataset)
# specify estimated diversity data
hbins_estD <- hbins_estD %>% mutate(estD = !!sym(glue("qD_{q}")))

# save results
dataset$hbins_estD <- hbins_estD
dataset$hbins_estD %>% saveRDS(file.path(output_dir,"hbins_estD.rds"))

# convert bin polygons to points (at centroid)
estD_pts <- hbins_estD %>% 
  filter(level==1) %>%
  st_centroid()

# export observed point values
exp_path <- file.path(output_dir,"geojson")
if (!dir.exists(exp_path)) {
  dir.create(exp_path) 
}
estD_pts %>% drop_na(estD) %>% st_write(file.path(exp_path,"estD_pts.geojson"))
if(arcgis) {
  write_arcgis(file.path(gdb, run_id, glue("{run_id}_estD_pts")), estD_pts)
  # additionally export records with nLists >= 5
  write_arcgis(file.path(gdb, 
                         run_id, 
                         glue("{run_id}_estD_pts_gt5")), 
               estD_pts %>% drop_na(estD) %>% filter(nLists >= 5))
}



### 4 - Krige estimated diversity over AOI ###

# ordinary kriging (gstat)
source("functions/4a - ordinary kriging.R")

OK_result <- execute_OK(hbins_estD = dataset$hbins_estD %>% filter(nLists >= 5), 
                        hbins = dataset$hbins,
                        lag_size = run_start_scale_m) %>% 
                        # bind result to full hbin set
                        bind_rows(dataset$hbins_estD %>% filter(level > 1))

# empirical Bayesian kriging (ArcGIS)
if(arcgis) {
  # prepare necessary input arguments
  args=c(file.path(gdb, run_id, glue("{run_id}_estD_pts_gt5")), # 
         "estD", 
         file.path(gdb, run_id, glue("{run_id}_estD_pts")), 
         file.path(output_dir,"arcgis","EBK.lyrx"), 
         file.path(gdb, run_id, glue("{run_id}_EBK")), 
         "--validate")
  # run a python script to execute empirical Bayesian kriging in ArcGIS
  system(command=glue('"{conda_path}" run -n working python',
                      '"functions\\4b\ -\ empirical\ Bayesian\ kriging.py" ',
                      '"{args[1]}" {args[2]} "{args[3]}"',
                      '"{args[4]}" "{args[5]}" {args[6]}'))
  # load kriging result
  EBK_result <- 
    read_arcgis(file.path(gdb, run_id, glue("{run_id}_EBK"))) %>% 
    st_drop_geometry() %>% 
    # replace point geometry with original polygon geometry
    mutate(geometry = dataset$hbins_estD %>% 
             filter(level==1) %>% .$geometry) %>% 
    st_as_sf() %>% 
    # bind result to full hbin set
    bind_rows(dataset$hbins_estD %>% filter(level > 1))
}

### 5 - Compute scale variance ###
source("functions/5 - compute scale variance.R")

# observed locations result
sv_OBS <- compute_scale_variance(
  dataset$hbins_estD %>% 
  dplyr::mutate(estD = replace(estD, nLists >= 5, NA)),
  dataset$hbins
)
sv_OBS %>% saveRDS(file.path(output_dir, "sv_OBS.rds"))

#  ordinary kriging result
sv_OK <- compute_scale_variance(
  OK_result %>% dplyr::mutate(estD = var1.pred),
  dataset$hbins
)
sv_OK %>% saveRDS(file.path(output_dir, "sv_OK.rds"))

# empirical Bayesian kriging result
if(arcgis) {
  sv_EBK <- compute_scale_variance(
    EBK_result %>% 
    dplyr::mutate(estD = Predicted),
    dataset$hbins
  )
  sv_EBK %>% saveRDS(file.path(output_dir, "sv_EBK.rds"))
}

# define function to export scale variance elements (sve) for each level
write_sve <- function(level, sve) {
  st_write(sve[[level]], 
           file.path(output_dir, "geojson", glue("sve{level}.geojson")))
  if(arcgis) {
    write_arcgis(file.path(gdb, run_id, glue("{run_id}_sve{level}")), 
                 sve[[level]])
  }
}
# export scale variance elements for each level
1:run_levels %>% map(\(l) write_sve(level=l, sve=sv_OBS$sve))

# explore scale variance components
sv_OBS$svc %>% write_csv(file.path(output_dir, "svc_OBS.csv"))
sv_OK$svc %>% write_csv(file.path(output_dir, "svc_OK.csv"))
sv_EBK$svc %>% write_csv(file.path(output_dir, "svc_EBK.csv"))
