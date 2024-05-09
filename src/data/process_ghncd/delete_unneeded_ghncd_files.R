sf_nyc_stations <- st_read(here("data", "interim", "ghcnd_stations_nyc.gpkg"))

nyc_station_ids <- sf_nyc_stations$ID

## Delete .dly files that are not in the NYC stations
dly_files <- list.files(here("data", "raw", "precip", "ghcnd_all"), pattern = ".dly$", full.names = TRUE)

## dly_files_to_delete <- dly_files[!dly_files %in% nyc_station_ids]
## delete files whose names do not contain any of the station IDs
dly_files_to_delete <-
  dly_files[!grepl(paste(nyc_station_ids, collapse = "|"), dly_files)]

file.remove(dly_files_to_delete)


