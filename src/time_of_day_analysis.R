sf_flooding_calls <- load_flooding_calls()

## Bar plot of number of flooding calls by time of day
total_calls <- nrow(sf_flooding_calls)

df_calls_per_hour <-
  sf_flooding_calls %>%
  filter(precip_in == 0) %>%
  mutate(hour_of_day = as.numeric(str_sub(time_of_day, 1, 2))) %>%
  group_by(hour_of_day) %>%
  summarise(calls_per_hour = n() / total_calls) %>%
  as_tibble %>%
  select(!geometry)

## lm(n ~ cos(hour_of_day * pi / 12), data = sf_flooding_calls_time_of_day) %>%
  ## summary

## cos_model <- lm(n ~ cos(hour_of_day * pi / 12), data = sf_flooding_calls_time_of_day)

## model <- lm(n ~ poly(hour_of_day, degree = 10), data = sf_flooding_calls_time_of_day)

sf_flooding_calls %>%
  mutate(hour_of_day = as.numeric(str_sub(time_of_day, 1, 2))) %>%
  ggplot(aes(x = hour_of_day)) +
  geom_bar(fill = "blue") +
  ## labs(title = "Number of Flooding Calls by Time of Day",
       ## x = "Hour of Day",
       ## y = "Number of Calls") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
