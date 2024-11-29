source("src/access_floodnet.R")

library(gganimate)
library(viridis)

fill_na_surrounding <- function(x) {
  # Get indices of NAs
  na_idx <- which(is.na(x))
  
  for(i in na_idx) {
    # Find nearest non-NA values before and after
    before <- x[seq(i, 1, -1)][!is.na(x[seq(i, 1, -1)])][1]
    after <- x[seq(i, length(x))][!is.na(x[seq(i, length(x))])][1]
    
    # Calculate replacement value
    if(is.na(before)) {
      # If at start, use next non-NA value
      x[i] <- after
    } else if(is.na(after)) {
      # If at end, use previous non-NA value
      x[i] <- before
    } else {
      # Otherwise use mean
      x[i] <- mean(c(before, after))
    }
  }
  
  return(x)
}

## start_time <- ymd_hms("2023-09-01T00:00:00-04:00")
## end_time <- ymd_hms("2023-09-30T00:00:00-04:00")

boundaries_path <- "/home/ian/projects/nyc_floods/data/raw/boundaries/borough_boundaries.gpkg"

sf_boundaries <- st_read(boundaries_path) %>%
  st_transform(2263)

start_time <- as.POSIXct("2023-09-29 00:00:00", tz = "EST")
end_time <- as.POSIXct("2023-09-30 00:00:00", tz = "EST")

depth_data <- query_depth_data(start_time=start_time, end_time=end_time)

deployments <- get_deployments()

sf_depth_data <- depth_data %>%
  inner_join(deployments, by="deployment_id") %>%
  select(deployment_id, time, depth_proc_mm, geometry) %>%
  st_as_sf() %>%
  st_transform(2263)

sf_depth_data$x <- st_coordinates(sf_depth_data$geometry)[,1]
sf_depth_data$y <- st_coordinates(sf_depth_data$geometry)[,2]

sf_depth_data$time_scaled <-
  (as.numeric(sf_depth_data$time) - as.numeric(start_time)) /
  (as.numeric(end_time) - as.numeric(start_time))

sf_depth_data$time_rounded <-
  round_date(sf_depth_data$time, "5 minutes")

sf_depth_data <-
  sf_depth_data %>%
  group_by(deployment_id) %>%
  arrange(time) %>%
  mutate(depth_smooth = fill_na_surrounding(depth_proc_mm) %>%
           zoo::rollmean(k = 5, fill = NA))

x_factor <- 1.5e4
y_factor <- 10

sf_depth_data %>%
  ggplot() +
  geom_line(aes(x = time,
                y = depth_smooth)) +
  facet_wrap(~deployment_id) 

exclude <- c("jolly_tender_squid",
             "early_still_frog",
             "tired_red_monkey",
             "kindly_expert_cobra",
             "openly_driven_tarpon")

## Add rounded time to df_model
df_model$time_rounded <- round_date(df_model$time, "5 minutes")

flooding_calls <- load_flooding_calls(start_time,
                                      end_time)

## copy calls so they "last" 15 minutes


sf_flooding_calls <- flooding_calls %>%
  st_as_sf() %>%
  st_transform(2263) %>%
  mutate(time_rounded = round_date(call_time, "5 minutes"))

sf_flooding_calls_expanded <- sf_flooding_calls %>%
  # Assuming there's a timestamp column, add rows for each point that extend its duration
  mutate(time_range = map(time_rounded, ~seq(.x, .x + minutes(30), by = "5 mins"))) %>%  # adjust duration as needed
  unnest(time_range) %>%
  mutate(time_rounded = time_range) %>%
  select(-time_range)
             
p_floodnet <-
  sf_depth_data %>%
  filter(!deployment_id %in% exclude) %>%
  group_by(deployment_id) %>%
  ggplot() +
  ## Borough boundaries as basemap
  geom_sf(data = sf_boundaries, fill = "grey", color = "black") +
  # Animated points showing depth
  geom_point(aes(x = x,
                 y = y,
                 fill = depth_smooth,
                 color = depth_smooth,
                 size = depth_smooth,
                 group = deployment_id),
             alpha = 0.75) +
  geom_sf(data = sf_flooding_calls_expanded, 
          aes(geometry = geometry, group = seq_along(geometry)), # add group aesthetic
          color = "red") +
  labs(title = "FloodNet Inundation Depth: 9/29/2023 Rain Event",
       subtitle = "Time: {stringr::str_pad(format(frame_time, '%Y-%m-%d %H:%M'), width = 20, side = 'right')}") +
  ## size scale
  scale_size_continuous(
    name = "Depth (mm)",
    range = c(1, 20),
    limits = c(0, max(sf_depth_data$depth_proc_mm, na.rm = TRUE))
  ) +
  theme_classic() +
  theme(
    plot.subtitle = element_text(size = 30,
                                 margin = margin(b = 10, t = 10),
                                 hjust = 0.5),
                                        # Fix the plot margins
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
                                        # Ensure title area has fixed height
    ## Bigger title
    plot.title = element_text(size = 40),
    ## No axes or axes labels
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    ## Bigger base font size
    text = element_text(size = 24)
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL) +
  guides(color = "none",
         fill = "none") +
  transition_time(time_rounded) +
  ease_aes('linear')

gganimate::animate(p_floodnet,
                   nframes = 100,
                   duration = 20,
                   width = 1400,
                   height = 1200,
                   renderer = gifski_renderer())

## Animate p_floodnet
anim <-
  p_floodnet +
  transition_time(time_rounded) +
  ease_aes('linear')

anim_save("floodnet.gif")
  
  # Custom scales for depth representation
  ## scale_color_viridis_c(
  ##   option = "plasma",
  ##   name = "Depth (mm)",
  ##   limits = c(0, max(sf_depth_data$depth_proc_mm, na.rm = TRUE))
  ## ) +
  ## scale_size_continuous(
  ##   name = "Depth (mm)",
  ##   range = c(1, 10),
  ##   limits = c(0, max(sf_depth_data$depth_proc_mm, na.rm = TRUE))
  ## ) +
  
  ## # Theme and labels
  ## theme_minimal() +
  
  # Animation settings
  transition_time(time) +
  ease_aes('linear')
