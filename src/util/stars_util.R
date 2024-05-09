add_calls_to_stage4_stars <- function(stars_stage4) {

  df_stage4 <- as_tibble(stars_stage4)

  sf_stage4 <-
    st_as_sf(stars_stage4,
             na.rm = FALSE,
             long = TRUE,
             as_points = FALSE) %>%
    rename(precip_time = time)

  ## add cell centroid x and y coordinates
  ## so we can join back to stars object later
  sf_stage4 <- cbind(sf_stage4,
                     get_stage4_coords(stars_stage4))

  if (!check_points_within_cells(sf_stage4)) {
    stop("Cell centroid coordinates aren't within cell polygons in sf object.")
  }

  ## join 311 calls to stage4
  sf_stage4_with_calls <-
    sf_stage4 %>%
    st_join(load_flooding_calls(dt_start,
                                dt_end),
            .predicate = st_within,
            left = TRUE,
            suffix = c(".precip", ".calls")) %>%
    filter(call_time >= precip_time &
           call_time < precip_time + hours(1))

  ## join back into the stars df based on x, y, and time,
  ## summarizing to get call counts
  df_stage4_with_call_count <-
    df_stage4 %>%
    left_join(as_tibble(sf_stage4_with_calls),
              join_by(x == x,
                      y == y,
                      time == precip_time,
                      hourly_precip == hourly_precip)) %>%
    group_by(x, y, time) %>%
    summarize(n_calls = sum(!is.na(descriptor)),
              hourly_precip = first(hourly_precip))

  ## convert back to stars object
  stars_stage4_with_calls <-
    df_stage4_with_call_count %>%
    st_as_stars(dims = c("x", "y", "time"))

  st_crs(stars_stage4_with_calls) <- st_crs(2263)

  return(stars_stage4_with_calls)
}

get_stage4_coords <- function(stars_stage4) {
  coords <-
    stars_stage4 %>%
    st_as_sf(na.rm = FALSE,
             long = TRUE,
             as_points = TRUE) %>%
    st_coordinates(geometry) %>%
    as_tibble %>%
    rename(x = X,
           y = Y)
  return(coords)
}

check_points_within_cells <- function(sf_stage4) {
  ## check that coords are within proper cells after cbind
  points <- st_as_sf(as_tibble(sf_stage4),
                     coords = c("x", "y"),
                     crs = 2263)

  centroids_within_cells <-
    all(map2_lgl(points$geometry,
                 sf_stage4$geometry,
                 \(point, cell) st_within(point, cell,
                                          sparse = FALSE)[[1]]))

  return(centroids_within_cells)
}
