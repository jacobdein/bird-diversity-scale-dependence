# Run Analysis
# This file defines the analysis steps and calls each part, 
# defined in the "functions" directory with the required parameters.


library(glue)
library(tidyverse)
library(arcgisbinding)


run_analysis <- function(run_id, run_start_scale_m, run_levels) {
  
  ### specify run parameters ###
  # specify an identifier to id the results from this set of parameters
  #run_id <- run_id
  # specify a name for the area of interest (aoi)
  run_location <- "London"
  # specify path to aoi geometry
  run_aoi <- "../data/GreaterLondon.geojson"
  # specify size in meters of largest scale
  #run_start_scale_m <- run_start_scale_m
  # specify the number of levels (scales)
  #run_levels <- run_levels
  # specify a full name for this set of parameters
  run <- glue("{run_id} - {run_location} {run_start_scale_m}m {run_levels}L")
  # specify the input observations
  run_obs <- "../data/obs_London.rds"
  # specify the output directory to save the results
  output_dir <- file.path("../results", run)
  
  # print full run name
  print(glue("Running: {run}"))
  
  # specify if arcgis functions are available and should be used
  # and conditionally set required parameters and load utility functions
  arcgis <- TRUE
  if(arcgis) {
    source("./utilities/arcgis.R")
    gdb <- file.path("..\\results\\ArcGIS\\scale_variance.gdb")
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
    write_arcgis(file.path(gdb, run_id, glue("{run_id}_hbins_1")), hbins)
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
  dataset$hbins_estD <- estimate_diversity(dataset)
  
  # loop through analysis of diversity estimates for each q
  for (q in c(0, 1, 2)) {
    cat(paste0("\nAnalysing estimated diversity, q = ",q,"\n"))
    
    # specify estimated diversity data
    dataset$hbins_estD <- dataset$hbins_estD %>% mutate(estD = !!sym(glue("qD_{q}")))
    
    # save results
    dataset$hbins_estD %>% saveRDS(file.path(output_dir,"hbins_estD.rds"))
    
    # convert bin polygons to points (at centroid)
    estD_pts <- dataset$hbins_estD %>% 
      filter(level==1) %>%
      st_centroid()
    
    # export observed point values
    exp_path <- file.path(output_dir,"geojson")
    if (!dir.exists(exp_path)) {
      dir.create(exp_path) 
    }
    estD_pts %>% drop_na(estD) %>% st_write(file.path(exp_path, glue("estD_pts_q{q}.geojson")))
    if(arcgis) {
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_estD_pts_q{q}")), estD_pts)
      # additionally export records with spread <= 12
      write_arcgis(file.path(gdb, 
                             run_id, 
                             glue("{run_id}_estD_pts_lt12_q{q}")), 
                   estD_pts %>% drop_na(estD) %>% filter(!!sym(glue("q{q}_spread")) <= 12))
    }
    
    
    
    ### 4 - Krige estimated diversity over AOI ###
    
    # ordinary kriging (gstat)
    source("functions/4a - ordinary kriging.R")
    
    OK_result <- execute_OK(hbins_estD = dataset$hbins_estD %>% filter(!!sym(glue("q{q}_spread")) <= 12), 
                            hbins = dataset$hbins,
                            lag_size = run_start_scale_m) %>% 
      # bind result to full hbin set
      bind_rows(dataset$hbins_estD %>% filter(level > 1)) %>% 
      dplyr::mutate(estD = var1.pred)
    OK_result$geometry <- hbins$geometry # change geometry from point to polygons
    
    # save ordinary kriging result
    OK_result %>% st_write(file.path(output_dir, "geojson", glue("OK_result_q{q}.geojson")))
    if (arcgis) {
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_OK_result_q{q}")), OK_result)
    }
    
    # empirical Bayesian kriging (ArcGIS)
    if(arcgis) {
      # prepare necessary input arguments
      args=c(file.path(gdb, run_id, glue("{run_id}_estD_pts_lt12_q{q}")), # 
             "estD", 
             file.path(gdb, run_id, glue("{run_id}_estD_pts_q{q}")), 
             file.path(output_dir,"arcgis",glue("EBK_q{q}.lyrx")), 
             file.path(gdb, run_id, glue("{run_id}_EBK_q{q}")), 
             "--validate")
      # run a python script to execute empirical Bayesian kriging in ArcGIS
      system(command=glue('"{conda_path}" run -n working python ',
                          '"functions\\4b\ -\ empirical\ Bayesian\ kriging.py" ',
                          '"{args[1]}" {args[2]} "{args[3]}" ',
                          '"{args[4]}" "{args[5]}" {args[6]}'))
      # load kriging result
      EBK_result <- 
        read_arcgis(file.path(gdb, run_id, glue("{run_id}_EBK_q{q}"))) %>% 
        st_drop_geometry() %>% 
        # replace point geometry with original polygon geometry
        mutate(geometry = dataset$hbins_estD %>% 
                 filter(level==1) %>% .$geometry) %>% 
        st_as_sf() %>% 
        # bind result to full hbin set
        bind_rows(dataset$hbins_estD %>% filter(level > 1)) %>% 
        dplyr::mutate(estD = Predicted)
      
      # save EBK result
      EBK_result %>% st_write(file.path(output_dir, "geojson", glue("EBK_result_q{q}.geojson")))
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_EBK_result_q{q}")), EBK_result)
    }
    
    ### 5 - Compute scale variance ###
    source("functions/5 - compute scale variance.R")
    
    # observed locations result
    sv_OBS <- compute_scale_variance(
      dataset$hbins_estD %>% 
        dplyr::mutate(estD = replace(estD, !!sym(glue("q{q}_spread")) > 12, NA)),
      dataset$hbins
    )
    sv_OBS %>% saveRDS(file.path(output_dir, glue("sv_OBS_q{q}.rds")))
    
    #  ordinary kriging result
    sv_OK <- compute_scale_variance(
      OK_result %>% dplyr::mutate(estD = var1.pred),
      dataset$hbins
    )
    sv_OK %>% saveRDS(file.path(output_dir, glue("sv_OK_q{q}.rds")))
    
    # empirical Bayesian kriging result
    if(arcgis) {
      sv_EBK <- compute_scale_variance(
        EBK_result, 
        dataset$hbins
      )
      sv_EBK %>% saveRDS(file.path(output_dir, glue("sv_EBK_q{q}.rds")))
    }
    
    # define function to export scale variance elements (sve) for each level
    write_sve <- function(level, sve) {
      st_write(sve[[level]], 
               file.path(output_dir, "geojson", glue("sve{level}_q{q}.geojson")))
      if(arcgis) {
        write_arcgis(file.path(gdb, run_id, glue("{run_id}_sve{level}_q{q}")), 
                     sve[[level]])
      }
    }
    # export scale variance elements for each level
    1:run_levels %>% map(\(l) write_sve(level=l, sve=sv_OBS$sve))
    
    # export scale variance components
    sv_OBS$svc %>% write_csv(file.path(output_dir, glue("svc_OBS_q{q}.csv")))
    sv_OK$svc %>% write_csv(file.path(output_dir, glue("svc_OK_q{q}.csv")))
    sv_EBK$svc %>% write_csv(file.path(output_dir, glue("svc_EBK_q{q}.csv")))
    
    # export estimated values for all levels
    sv_OBS$estD_values %>% st_write(file.path(output_dir, "geojson", glue("OBS_estD_values_q{q}.geojson")))
    sv_OK$estD_values %>% st_write(file.path(output_dir, "geojson", glue("OK_estD_values_q{q}.geojson")))
    sv_EBK$estD_values %>% st_write(file.path(output_dir, "geojson", glue("EBK_estD_values_q{q}.geojson")))
    if (arcgis) {
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_OBS_values_q{q}")), sv_OBS$estD_values)
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_OK_values_q{q}")), sv_OK$estD_values)
      write_arcgis(file.path(gdb, run_id, glue("{run_id}_EBK_values_q{q}")), sv_EBK$estD_values)
    }
  }
}
