library(here)
library(stars)
source(here("src", "util", "stage4_util.R"))
source(here("src", "util", "data_util.R"))
source(here("src", "util", "stars_util.R"))
source(here("src", "util", "cumulative_precip_util.R"))

dt_start <- as.POSIXct("2021-09-01 06:00:00", tz = "EST")
dt_end <- dt_start + days(3)

data_dir <- here("data",
                 "raw",
                 "precip",
                 "nexrad",
                 "stage4")

stars_precip_calls_orig <- load_precip_calls_stars(dt_start, 
                                                   dt_end,
                                                   data_dir,
                                                   disagg = TRUE)

stars_precip_calls <- 
  stars_precip_calls_orig %>%
  mutate(any_call = n_calls > 0)

coords_in_nyc <-
  st_as_sf(stars_precip_calls,
           long = TRUE,
           as_points = TRUE) %>%
  group_by(geometry) %>%
  summarize() %>%
  st_filter(load_nyc_boundary(), .predicate = st_within) %>%
  pull(geometry) %>%
  st_coordinates %>%
  as_tibble(coords_in_nyc) %>%
  rename(x = X, y = Y) %>%
  mutate(in_nyc = TRUE)

stars_precip_calls <- 
  stars_precip_calls %>%
  as_tibble %>%
  left_join(coords_in_nyc, join_by(x, y)) %>%
  st_as_stars(dims = c("x", "y", "time"))

stars_precip_calls <-
  stars_precip_calls %>%
  as_tibble %>%
  ## replace NA with FALSE in in_nyc
  mutate(in_nyc = if_else(is.na(in_nyc), FALSE, in_nyc)) %>%
  st_as_stars(dims = c("x", "y", "time"))

## add cumulative precipitation
stars_precip_calls <-
  stars_precip_calls %>%
  as_tibble %>%
  group_by(x, y) %>%
  arrange(time) %>%
  mutate(cumulative_precip =
           cumsum(hourly_precip),
         precip_decay_sum =
           calc_decay_sum(hourly_precip, 3),
         precip_decay_mult =
            calc_decay_mult(hourly_precip, .95)) %>%
  st_as_stars(dims = c("x", "y", "time"))

## add time of day adjustment
stars_precip_calls <- 
  stars_precip_calls %>%
  as_tibble() %>%
  mutate(hour_of_day = hour(time)) %>%
  left_join(df_calls_per_hour,
            join_by(hour_of_day)) %>%
  st_as_stars(dims = c("x", "y", "time"))

st_crs(stars_precip_calls) <- st_crs("EPSG:2263")



model <- glm(any_call ~ hourly_precip +
               precip_decay_mult +
               calls_per_hour,
             data = df_precip_calls[df_precip_calls$in_nyc, ],
             family = "binomial")

df_precip_calls$pred <- predict(model, type = "response")

df_precip_calls %>%
  ggplot(aes(y = y, x = x, fill = hourly_precip)) +
  facet_wrap(~time) +
  geom_raster() +
  theme_minimal() +
  scale_fill_viridis_c() +
  load_nyc_boundary_layer("red")

stars_precip_calls %>%
  as_tibble %>%
  filter(n_calls > 0) %>%
  ggplot(aes(y = n_calls,
             x = precip_decay_mult)) +
  geom_point(alpha = 1/10,
             aes(color = time)) +
  geom_smooth(method = "lm") +
  scale_y_log10()

lm_precip <-
  stars_stage4_calls %>%
  as_tibble %>%
  mutate(hour_of_day = hour(time)) %>%
  left_join(df_calls_per_hour, join_by(hour_of_day)) %>%
  filter(n_calls > 0) %>%
  lm(log(n_calls) ~
       lag(precip_decay_mult, 1) +
       lag(hourly_precip, 3) +
       calls_per_hour,
     .)




sf_boundary <- load_nyc_boundary()

base_plot <- 
  stars_stage4_calls %>%
  as_tibble %>%
  ggplot(aes(y = y, x = x)) +
  facet_wrap(~time) +
  scale_fill_viridis_c() +
  theme_minimal() 

nyc_boundary <-
  geom_sf(aes(x = NULL,
              y = NULL),
          data = load_nyc_boundary(),
          color = "black",
          linewidth = 1.5,
          alpha = .75,
          fill = NA)

p_precip <-
  base_plot +
  geom_raster(aes(fill = log10(hourly_precip))) +
  nyc_boundary

p_calls <-
  base_plot +
  geom_raster(aes(fill = log10(n_calls))) +
  nyc_boundary

plot_grid(p_calls, p_precip)

p_mapped_plots <-
  stars_stage4_calls %>%
  as_tibble() %>%
  group_by(x, y) %>%
  arrange(time) %>%
  mutate(cumulative_precip = cumsum(hourly_precip)) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = n_calls)) +
  geom_line(aes(y = hourly_precip / 3),
            color = "blue") +
  ## scale_x_log10() +
  facet_grid(rows = vars(-x), cols = vars(y)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_c() 

## xlim(c(0, 1)) +

stars_stage4_calls %>%
  as_tibble %>%
  filter(between(time,
                 dt_start,
                 dt_start + hours(24))) %>%
  group_by(time) %>%
  summarize(total_precip = sum(hourly_precip),
            total_calls = sum(n_calls),
            precip_decay_mult = sum(precip_decay_sum)) %>%
  ggplot(aes(y = total_calls,
             x = time)) +
  geom_line() +
  geom_line(aes(y = total_precip / 10),
            color = "blue") 
  ## geom_line(aes(y = precip_decay_mult),
            ## color = "red") 


stars_stage4_calls %>%
  as_tibble() %>%
  filter(n_calls > 0) %>%
  group_by(x, y) %>%
  arrange(time) %>%
  mutate(cumulative_precip = cumsum(hourly_precip)) %>%
  ungroup() %>%
  filter(cumulative_precip <= 4) %>%
  group_by(x, y) %>%
  mutate(is_max = (n_calls == max(n_calls))) %>%
  filter(is_max,
         n_calls > 10) %>%
  ungroup() %>%
  ggplot(aes(x = x,
             y = y,
             fill = cumulative_precip / 25.4)) +
  geom_raster() +
  nyc_boundary +
  scale_fill_viridis_c(direction = -1)
  
## group by cell_id and precip_time
## for groups that have some values where call_within_cell == TRUE, filter out any rows where call_within_cell == FALSE
  
sf_stage4_with_calls_summary <-
  sf_stage4_with_calls %>%
  ## st_filter(load_nyc_boundary(), .predicate = st_overlaps) %>%
  mutate(is_call = !is.na(descriptor)) %>%
  group_by(geometry, precip_time) %>%
  summarise(n_calls = sum(is_call),
            hourly_precip = first(hourly_precip),
            cell_id = first(cell_id)) %>%
  ungroup() %>%
  group_by(geometry) %>%
  arrange(precip_time) %>%
  mutate(cumulative_precip = cumsum(hourly_precip),
         n_calls_diff = c(NA, diff(n_calls))) %>%
  ungroup()

sf_stage4_with_calls_summary %>%
  mutate(cumulative_precip_in = cumulative_precip / 25.4) %>%
  group_by(cumulative_precip_in,
           hourly_precip,
           cell_id) %>%
  summarize(n_calls_diff = max(n_calls_diff),
            n_calls = max(n_calls),
            precip_time = min(precip_time)) %>%
  ## summarize(n_calls_diff = max(n_calls_diff),
            ## cumulative_precip = max(cumulative_precip),
            ## cell_id = first_
  ggplot(aes(y = n_calls, x = precip_time)) +
  ## scale_x_log10() +
  ## geom_point() +
  ## scale_color_viridis_c() +
  theme_minimal() +
  ## facet_wrap(~cell_id,
             ## ncol = 7) +
  ## geom_smooth(method = "loess",
              ## se = FALSE,
              ## span = .5) +
  ## facet_wrap(~cell_id) +
  ## geom_smooth(method = "loess",
              ## se = FALSE,
              ## span = .1) +
  geom_line(aes(color = precip_time,
                group = cell_id)) 
  ## geom_label(aes(label = cell_id))

  ## xlim(c(0, 50))

sf_stage4_with_calls_summary %>%
  ggplot(aes(y = n_calls, x = datetime)) +
  geom_point() 

## Plotting
p_precip <- 
  df_stage4 %>%
  ## filter(time >= as.POSIXct("2021-09-01 08:00:00", tz = "EST")) %>%
  mutate(cumulative_precip_in = cumulative_precip / 25.4) %>%
  ggplot() +
  geom_raster(aes(x = x,
                  y = y,
                  fill = log10(hourly_precip))) +
  facet_wrap(~time,
             ncol = 7) +
  scale_fill_viridis_c() +
  geom_sf(data = load_nyc_boundary(),
          fill = NA,
          color = "white") +
  theme_void() +
  theme(legend.position = "bottom") 
  
p_calls <-
  sf_stage4_with_calls_summary %>%
  ggplot() +
  geom_sf(aes(fill = n_calls)) +
  facet_wrap(~precip_time,
             ncol = 7) +
  scale_fill_viridis_c() +
  theme_void() +
  geom_sf(data = load_nyc_boundary(),
          color = "red",
          fill = NA) +
  theme(legend.position = "bottom")
  
