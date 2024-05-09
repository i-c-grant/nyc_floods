data_dir <- here("data", "raw", "precip", "ghcnd_nyc")

ghcnd_csvs <- list.files(data_dir, pattern = "csv$", full.names = TRUE)

df_precip <-
  read_csv(ghcnd_csvs) %>%
  filter(element == "PRCP") %>%
  ## pivot longer so that e.g. Val1 is the value for the first day of the month
  pivot_longer(cols = starts_with("Val"), names_to = "day", values_to = "precip") %>%
  mutate(day = as.numeric(str_remove(day, "Val")),
         date = make_date(year, month, day)) %>%
  filter(date >= "2010-01-01") %>%
  rename(station = ID) %>%
  select(!1)

df_precip %>%
  ggplot(aes(date, precip)) +
  geom_point(aes(color = ID)) +
  labs(title = "Daily precipitation in NYC",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_minimal()

## calculate how many days have passed since 2010-01-01
total_days <- today() - as.Date("2010-01-01")

sf_nyc_stations <- st_read(here("data", "interim", "ghcnd_stations_nyc.gpkg"))

## plot total number of days with non-NA precipitation per station
df_precip %>%
  group_by(station) %>%
  summarize(n_days = sum(!is.na(precip))) %>%
  mutate(pct_days = n_days / as.numeric(total_days)) %>%
  left_join(sf_nyc_stations, join_by(station == ID)) %>%
  st_sf() %>%
  mapview(zcol = "pct_days", legend = TRUE)

## Central Park station is best
central_park_station <- "USW00094728"

## Plot just Central Park precipitation
df_precip %>%
  filter(station == central_park_station) %>%
  ggplot(aes(date, log10(precip))) +
  geom_point() +
  labs(title = "Daily precipitation in Central Park",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_minimal()

df_precip %>%
  filter(station == central_park_station) %>%
  ggplot(aes(log10(precip))) +
  geom_histogram(binwidth = .1) +
  theme_minimal() 

## Write CSV with just Central Park data
df_precip %>%
  filter(station == central_park_station) %>%
  write_csv(here("data", "processed", "central_park_precip.csv"))
