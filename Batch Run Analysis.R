# Run Analysis
# Jacob Dein
# September 2023

#library(furrr)
library(purrr)

# set parallelization options
#plan(multisession)
#foptions <- furrr_options(seed = 1)

# specify run parameters
run_ids <- c("R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10")
start_scales <- c(500, 640, 750, 1000, 1280, 1500, 2000, 2560, 3000)
nlevels <- c(8, 8, 7, 7, 7, 6, 6, 6, 5)

#1 320 9
#2 500 8
#3 640 8
#4 750 7
#5 1000 7
#6 1280 7
#7 1500 6 
#8 2000 6
#9 2560 6
#10 3000 5

# source analysis function
source("./Run Analysis Function.R")

# run analyses with specified parameters with parallelized pmap function
r <- pmap(list(run_ids, start_scales, nlevels), run_analysis)
