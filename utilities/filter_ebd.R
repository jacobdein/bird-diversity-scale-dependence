# load packages
library(auk)
library(lubridate)
library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select

#### initial filtering performed in colab
# # setup data directory
# setwd("..\data")
# dir.create("processed", showWarnings = FALSE)
# 
# # set raw data source
# ebd <- auk_ebd("../data/EBD/Dec-2023/ebd_GB_smp_relDec-2023.txt", 
#                file_sampling = "../data/EBD/Dec-2023/ebd_GB_smp_relDec-2023_sampling.txt")
# 
# # filter data
# ebd_filters <- ebd %>%
#   auk_bbox(bbox = c(-1.4177, 50.7000, 1.2414, 52.2661)) %>%
#   auk_date(date = c("2014-01-01", "2024-01-01")) %>%
#   # restrict to the standard traveling and stationary count protocols
#   auk_protocol(protocol = c("Stationary", "Traveling")) %>%
#   auk_complete()
# ebd_filters
# 
# # set output files
data_dir <- "../data/EBD/Dec-2023/processed"
if (!dir.exists(data_dir)) {
   dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_London.txt")
f_sampling <- file.path(data_dir, "ebd_checklists_London.txt")

# save filtered EBD
# only run if the files don't already exist
# otherwise, read existing file
if (!file.exists(f_ebd)) {
  ebd_df <- ebd_filters %>%
    auk_filter(file = f_ebd, file_sampling = f_sampling) %>%
    read_ebd(unique=TRUE, rollup=TRUE)
} else {
  ebd_df <- read_ebd("../data/EBD/Dec-2023/processed/ebd_London.txt", unique = TRUE, rollup = TRUE)
}

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
ebd_c <- ebd_df %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    # effort_distance_km = if_else(protocol_type != "Traveling", 
    #                              0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

# preview data
ebd_c %>% glimpse()

# save zero-filled observations
ebird <- ebd_c %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name, common_name, exotic_code,
         observation_count, #species_observed, 
         locality, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km, effort_area_ha,
         number_observers, trip_comments, species_comments)
write_csv(ebird, "../data/EBD/Dec-2023/processed/ebd_London.csv", na = "")
