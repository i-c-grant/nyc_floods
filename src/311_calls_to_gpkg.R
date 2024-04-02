library(tidyverse)
library(here)
library(sf)
library(janitor)
library(mapview)

calls_sept_23 <- read_csv(here("data", "flooding_catch_basin_sept_2023.csv")) %>%
    clean_names()

calls_all_flooding <- read_csv(here("data", "311_dep_contains_flooding_all_dates.csv")) %>%
    clean_names()

sf_calls_sept_23 <- calls_sept_23 %>%
    ## format:"10/01/2023 09:44:00 PM"
    mutate(created_datetime = as_datetime(created_date,
                                          format = "%m/%d/%Y %I:%M:%S %p"),
           closed_datetime = as_datetime(closed_date,
                                         format = "%m/%d/%Y %I:%M:%S %p")) %>%
    filter(!is.na(longitude),
           !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(4326)

sf_calls_all_flooding <- calls_all_flooding %>%
    mutate(created_datetime = as_datetime(created_date,
                                          format = "%m/%d/%Y %I:%M:%S %p"),
           closed_datetime = as_datetime(closed_date,
                                         format = "%m/%d/%Y %I:%M:%S %p")) %>%
    filter(!is.na(longitude),
           !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(4326)

## write to geopackage
st_write(sf_calls_sept_23, here("data", "flooding_catch_basin_sept_2023.gpkg"),
         append = FALSE)

st_write(sf_calls_all_flooding, here("data", "311_dep_contains_flooding_all_dates.gpkg"),
         append = FALSE)
