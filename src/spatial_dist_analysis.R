sf_flooding_calls <- load_flooding_calls()

## Bar plot of number of flooding calls by time of day
total_calls <- nrow(sf_flooding_calls)

sf_cells <- stars_precip_calls %>%
  st_as_sf(long = TRUE) %>%
  select(geometry) %>%
  group_by(geometry) %>%
  summarize()

sf_n_calls_per_cell <- 
  sf_cells %>%
  st_join(sf_flooding_calls %>% filter(precip_in == 0),
          .predicate = st_within) %>%
  group_by(geometry) %>%
  filter(!is.na(descriptor)) %>%
  summarize(total_calls_in_cell = n())

