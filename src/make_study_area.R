sf_bk_queens <-
    read_sf(here("data",
                 "raw",
                 "boundaries",
                 "nyc_boroughs",
                 "nyc_boroughs.shp")) %>%
    filter(boro_name %in% c("Brooklyn", "Queens")) %>%
    st_transform(2263) 

sf_bk_q_big_parks <-
    read_sf(here("data",
                 "raw",
                 "boundaries",
                 "parks",
                 "bk_q_parks",
                 "bk_queens_parks.shp")) %>%
    st_transform(2263) %>%
    mutate(area = st_area(geometry) %>% as.numeric,
           perimeter = lwgeom::st_perimeter(geometry) %>% as.numeric,
           sliver_ratio = area / perimeter) %>% 
    filter(area > 1000^2,
           sliver_ratio > 100) %>%
    st_union()

read_drawn_boundary <- function(layer) {
    read_sf(here("data",
                 "processed",
                 "boundaries",
                 "drawn_boundaries.gpkg"),
            layer = layer)
}

sf_lower_manhattan <- read_drawn_boundary("lower_manhattan")
sf_jamaica_bay <- read_drawn_boundary("jamaica_bay")

sf_study_area <-
    st_difference(sf_bk_queens, sf_jamaica_bay) %>%
    st_difference(sf_bk_q_big_parks)
