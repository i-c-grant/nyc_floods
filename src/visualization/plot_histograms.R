df_calls_daily <- 
  read_csv(here("data",
                "processed",
                "311",
                "precip_responsive_calls_daily.csv"))

df_calls_daily_agg <-
  df_calls_daily %>%
  group_by(created_date) %>%
  summarise(total_daily_calls = first(total_daily_calls),
            precip_in = first(precip_in))

## Histogram of daily precipitation
df_calls_daily_agg %>%
  ggplot(aes(x = precip_in)) +
  geom_histogram(bins = 20) +
  labs(title = "Daily precipitation, 2010–2024 (Central Park station, GHCND)",
       x = "Precipitation (in)",
       y = "Number of days") +
  ## scale_x_log10() +
  xlim(c(NA, 4)) +
  ## ylim(c(0, 200)) +
  theme_classic()

df_calls_daily_agg %>%
  ggplot(aes(x = total_daily_calls)) +
  geom_histogram(bins = 60) +
  labs(title = "Daily precipitation-responsive 311 calls, 2010–2024",
       x = "Number of calls",
       y = "Number of days") +
  ## scale_x_log10() +
  xlim(c(NA, 500)) +
  ## ylim(c(0, 200)) +
  theme_classic()

## Add .5-inch precipitation bins to data
df_calls_daily_agg <- 
  df_calls_daily_agg %>%
  mutate(precip_in_bin = cut(precip_in,
                             breaks = seq(0, 8, by = 1),
                             include.lowest = TRUE))

n_bins <- 25
x_max <- 1000
 
p_311_calls_prcp_all <-
  df_calls_daily_agg %>%
  ggplot(aes(x = total_daily_calls)) +
  geom_histogram(bins = n_bins,
                 position = "dodge") +
  labs(title = "Daily 311 calls, all days (2010–2024)",
       x = "Number of calls",
       y = "Number of days") +
  xlim(c(NA, x_max)) +
  theme_classic() +
  geom_vline(xintercept = 40,
             linetype = "dashed",
             color = "red",
             linewidth = 1) 

## histogram with precip >= 1
p_311_calls_prcp_gt_1 <-
  df_calls_daily_agg %>%
  filter(precip_in > 1) %>%
  ggplot(aes(x = total_daily_calls)) +
  geom_histogram(bins = n_bins,
                 aes(fill = precip_in_bin)) +
  labs(title = "Daily 311 calls, days with precipitation ≥ 1 in (2010–2024)",
       x = "Number of calls",
       y = "Number of days") +
  xlim(c(NA, x_max)) +
  theme_classic() +
  geom_vline(xintercept = 40,
             linetype = "dashed",
             color = "red",
             linewidth = 1) +
  scale_fill_viridis_d(begin = .1) +
  labs(fill = "Precipitation (in)")

p_hist <-
  plot_grid(p_311_calls_prcp_all,
            p_311_calls_prcp_gt_1,
            ncol = 1,
            align = "v")

ggsave(here("reports",
            "figures",
            "hist_311_calls.png"),
       p_hist,
       width = 8,
       height = 6,
       dpi = 600)

lm_311_precip <-
  df_calls_daily_agg %>%
  filter(precip_in > 1) %>%
  lm(total_daily_calls ~ precip_in, data = .)

lm_label <- 
  paste0("y = ",
        round(coef(lm_311_precip)[1], 2),
        " + ",
        round(coef(lm_311_precip)[2], 2),
        "x")

## use expression for formatting
r2_label <- 
  expression(R^2 == 0.53)

p_lm_311_precip <-
  df_calls_daily_agg %>%
  filter(precip_in > 1) %>%
  ggplot(aes(y = total_daily_calls,
             x = precip_in)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm",
              se = FALSE) +
  ## include all labels on x axis
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  labs(title = "311 calls vs. precip. on days with precip. ≥ 1 in (2010–2024)",
       x = "Precipitation (in)",
       y = "Number of calls") +
  annotate("text", x = 3, y = 1800, label = lm_label, size = 6) +
  annotate("text", x = 3, y = 1700, label = r2_label, size = 6)

ggsave(here("reports",
            "figures",
            "lm_311_precip.png"),
       p_lm_311_precip,
       width = 8,
       height = 6,
       dpi = 600)

plot_grid(p_311_calls_prcp_all,
          p_311_calls_prcp_gt_1,
          nrow = 1,
          align = "v")

hist_data <- hist(df_calls_daily_agg$total_daily_calls,
                  breaks = n_bins,
                  plot = FALSE)
