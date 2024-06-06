# Batch Run Analysis
# This file calls "1 - Run Analysis Function.R" to run the analysis
# one or more times per the specified parameters.


library(purrr)

# specify run parameters
run_ids <- c("R1", "R2", "R3", "R4", "R5", "R6")
start_scales <- c(500, 750, 1000, 1500, 2000, 3000)
nlevels <- c(8, 7, 7, 6, 6, 5)

#1 500 8
#2 750 7
#3 1000 7
#4 1500 6 
#5 2000 6
#6 3000 5

# source analysis function
source("./1 - Run Analysis Function.R")

# run analyses with specified parameters
r <- pmap(list(run_ids, start_scales, nlevels), run_analysis)
