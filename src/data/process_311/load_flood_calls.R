source(here("src", "util", "data_util.R"))

df_sewer_calls <-
  load_sewer_calls() %>%
  ## load only the descriptors that are plausibly related to precipitation
  filter(descriptor %in% sewer_descriptors)

## Summarize daily sewer calls by descriptor and precipitation
df_sewer_calls_daily <-
  df_sewer_calls_precip %>%
  group_by(created_date, descriptor) %>%
  summarise(n_calls = n(),
            precip_in = first(precip_in))

sf_sewer_precip_summary <-
  sf_sewer_precip %>%
  group_by(created_date) %>%
  summarise(n_complaints = n(),
            precip = first(precip))

sf_sewer_precip_summary %>%
  ggplot(aes(x = precip, y = n_complaints)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  xlim(c(100, 2000))

sf_sewer_precip_summary_high <- 
  sf_sewer_precip_summary %>%
  filter(precip > 100) 

## sf_sewer_precip_summary_high %>%
sf_sewer_precip_summary %>%
  filter(n_complaints < 3000) %>%
  lm(n_complaints ~ precip, data = .) %>%
  summary

## histogram of number of complaints per day
sf_sewer_calls %>%
  as_tibble() %>%
  group_by(created_date) %>%
  summarise(n = n(),
            precip = first(precip)) %>%
  ggplot() + 
  geom_histogram(aes(x = n), binwidth = 10) +
  xlim(c(0, 500)) +
  theme_minimal()

## write flood calls to gpkg
sf_sewer %>%
  st_write(here("data", "processed", "flood_calls.gpkg"),
           driver = "GPKG")

## Write to csv
## All calls
write_csv(df_precip_calls_with_cp_precip,
          here("data",
               "processed",
               "311",
               "precip_responsive_calls.csv"))

## Daily summary
write_csv(df_precip_calls_daily,
          here("data",
               "processed",
               "311",
               "precip_responsive_calls_daily.csv"))
