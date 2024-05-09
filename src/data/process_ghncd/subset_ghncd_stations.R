## Read in station metadata
## code from http://spatialreasoning.com/wp/20170307_1244_r-reading-filtering-weather-data-from-the-global-historical-climatology-network-ghcn
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                  "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )

stations <- read.fortran(here("data",
                          "raw",
                          "precip",
                          "ghcnd_stations.txt"),
                     typedcols, 
                     comment.char="")

hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")

names(stations) <- hdrs

stations <- tibble::as_tibble(stations)

crs_state_plane_ny <- 2263

sf_stations <-
  stations %>% 
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  st_transform(crs_state_plane_ny)

## Define NYC location and map to verify
st_point(nyc_lon_lat) %>%
  mapview()

## Get stations within boundaries of NYC
sf_boros <-
  st_read(here("data",
               "raw",
               "boundaries",
               "borough_boundaries.gpkg")) %>%
  st_transform(crs_state_plane_ny)

sf_boros %>% mapview()

sf_stations %>%
  st_filter(sf_boros,
            .predicate = st_within) %>%
  st_write(here("data",
                "interim",
                "ghcnd_stations_nyc.gpkg"))

