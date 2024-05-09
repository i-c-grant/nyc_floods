library(ggmap)

sf_311 <- 
  read_csv(here("data", "processed", "311", "precip_responsive_calls.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

df_311_daily <-
  read_csv(here("data", "processed", "311", "precip_responsive_calls_daily.csv"))

sf_311 %>%
  filter(precip_in > 5) %>%
  mutate(created_date = as.factor(created_date)) %>%
  select(created_date) %>%
  plot

bbox <- st_bbox(sf_nyc)

311_boundary <- st_read(here("data",
                             "raw",
                             "boundaries",
                             "borough_boundaries.gpkg"))

## Plot 311 calls for precip > 2 in, faceted by created_date
p_maps <- 
  sf_311 %>%
  filter(precip_in > 2) %>%
  group_by(created_date) %>%
  mutate(total_n_calls = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_sf(size = 1.25,
          alpha = .5,
          aes(color = descriptor)) +
  facet_wrap(vars(created_date)) +
  theme_minimal() +
  labs(title = "311 calls on days with precip. > 2 in") +
  geom_sf(data = nyc_boundary, fill = NA, color = "black", size = 1) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ## add labels with daily total calls and precip
  geom_text(data = df_311_daily %>%
               filter(precip_in >= 2),
             aes(x = -74.25,
                 y = 40.85,
                 label = paste(total_daily_calls,
                               "calls\n",
                               round(precip_in, 2),
                               " in")),
             size = 3,
             color = "black",
               hjust = 0,
             vjust = 1)

ggsave(here("reports", "figures", "maps_2_in.png"), p_maps)

p_bar <- 
  df_311_daily %>%
  filter(precip_in >= 2) %>%
  ggplot() +
  geom_col(aes(y = precip_in * 500,
               x = 1),
           fill = "blue") +
  geom_col(aes(y = total_daily_calls,
               x = 2),
           fill = "red") +
  facet_wrap(~created_date) 

plot_grid(p_maps, p_bar)

sf_311 %>%
  mutate(time_of_day = hour(created_date)) 
  ggplot(aes(x = time_of_day)) +
  theme_minimal()

p_calls_3_in <-
  sf_311 %>%
  filter(precip_in > 3) %>%
  plot_daily_complaint_maps(title = "311 calls on 2016-01-03")

p_calls_2_in <-
  sf_311 %>%
  filter(precip_in > 2) %>%
  plot_daily_complaint_maps(title = "311 calls on 2016-01-03")

## calls with no precip
p_calls_no_precip <- 
  sf_311 %>%
  group_by(created_date) %>%
  mutate(total_daily_calls = n()) %>%
  ungroup() %>%
  filter(precip_in == 0,
         total_daily_calls > 200) %>%
  plot_daily_complaint_maps(title = "311 calls on 2016-01-03") 

ggsave(here("reports", "figures", "maps_no_precip.png"), p_calls_no_precip, dpi = 600)
ggsave(here("reports", "figures", "maps_3_in.png"), p_calls_3_in, dpi = 600)
ggsave(here("reports", "figures", "maps_2_in.png"), p_calls_2_in, dpi = 600)
