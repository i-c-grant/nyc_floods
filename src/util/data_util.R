library(tidyverse)
library(here)
library(sf)
library(janitor)

get_descriptor_abbrevs <- function() {
  df_descriptor_abbrevs <-
    
    tibble::tribble(
              ~descriptor,                                     
              ~descriptor_short,           
              ~descriptor_plot_label,

              "Catch Basin Clogged/Flooding (Use Comments) (SC)", 
              "clogged_catch",   
              "Basin clogged",

              "Street Flooding (SJ)",                            
              "street_flood",    
              "Street flooding",

              "Sewer Backup (Use Comments) (SA)",                
              "sewer_backup",    
              "Sewer backup",

              "Manhole Overflow (Use Comments) (SA1)",           
              "manhole_overflow",
              "Manhole overflow",

              "Culvert Blocked/Needs Cleaning (SE)",             
              "blocked_culvert", 
              "Blocked culvert",

              "Manhole Cover Missing (Emergency) (SA3)",         
              "missing_manhole", 
              "Manhole cover",

              "Catch Basin Grating Missing (SA4)",               
              "missing_grating", 
              "Basin grating",
              
              "Highway Flooding (SH)",                           
              "highway_flood",   
              "Highway flooding"
            )  

  return(df_descriptor_abbrevs)
}

load_sewer_calls <- function() {
  df_sewer_calls <-
    read_csv(here("data",
                  "raw",
                  "311",
                  "dep_all_sewer_20240425.csv")) %>%
    clean_names() %>%
    filter(!is.na(descriptor),
           !is.na(latitude),
           !is.na(longitude)) %>%
    mutate(created_datetime = parse_date_time(created_date,
                                              orders = "mdy HMS p",
                                              tz = "EST"),
           created_date = date(created_datetime),
           time_of_day = format(created_datetime, "%H:%M:%S")) %>%
    rename(call_time = created_datetime)

  ## Join daily precipitation data from Central Park station
  df_central_park_precip <- load_central_park_precip()

  df_sewer_calls_precip <-
    df_sewer_calls %>%
    left_join(df_central_park_precip, by = c("created_date" = "date")) %>%
    mutate(precip_mm = precip * .1,
           precip_in = precip_mm * 0.03937008)

  ## Join descriptor abbreviations
  df_descriptor_abbrevs <- get_descriptor_abbrevs()

  df_sewer_calls_precip <-
    df_sewer_calls_precip %>%
    left_join(df_descriptor_abbrevs, by = "descriptor")

  ## Convert to sf
  sf_sewer_calls_precip <- 
    df_sewer_calls_precip %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326,
             remove = FALSE) %>%
    st_transform(crs = 2263)

  return(sf_sewer_calls_precip)
}

load_flooding_calls <- function(dt_start = NA,
                                dt_end = NA) {

  df_flooding_calls <- load_sewer_calls()

  precip_descriptors <-
    c("Catch Basin Clogged/Flooding (Use Comments) (SC)",
      "Street Flooding (SJ)",
      "Sewer Backup (Use Comments) (SA)")
  
  if(is.na(dt_start) && is.na(dt_end)) {
    within_period <- TRUE
  }
  else {
    within_period <- between(df_flooding_calls$call_time,
                             dt_start,
                             dt_end)
  }

  df_flooding_calls <-
    df_flooding_calls %>%
    filter(descriptor %in% precip_descriptors,
           within_period)
  
  return(df_flooding_calls)
}

load_central_park_precip <- function() {
  df_central_park_precip <- read_csv(here("data",
                                          "processed",
                                          "central_park_precip.csv"))
  return(df_central_park_precip)
}

load_nyc_boundary <- function(layer = "nyc_state_plane") {
  nyc_boundary <- st_read(here("data",
                                "raw",
                                "boundaries",
                               "borough_boundaries.gpkg"),
                          quiet = TRUE,
                          layer = layer)
  return(nyc_boundary)
}
