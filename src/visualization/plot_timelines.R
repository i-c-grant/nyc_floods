source("src/util/viz_util.R")

start_date <- "2020-01-01"
end_date <- "2021-01-01"

df_calls_daily <-
  read_csv(here("data",
              "processed",
              "311",
              "precip_responsive_calls_daily.csv"))


## Plot timelines for each year from 2010 to 2024
## using map
years <- 2010:2024

plots <- map(years,
             ~plot_timeline(df = df_calls_daily,
                            start_date = paste0(.x, "-01-01"),
                            end_date = paste0(.x + 1, "-01-01"),
                            ymax = 4000))

## Save each plot to a file
map2(plots, years,
     ~ggsave(here("reports",
                  "figures",
                  "eda",
                  "timelines",
                  paste0("timeline_", .y, ".png")),
             plot = .x,
             width = 10,
             height = 6))

## Timeline of 9-1-2021 event by call descriptor
p_sept_2021 <- 
  plot_timeline(df_calls_daily,
                start_date = "2021-08-28",
                end_date = "2021-9-05",
                aggregate_descriptors = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "vertical") +
  ggtitle("Precipitation and 311 calls during Hurricane Ida") +
  
ggsave(here("reports", "figures", "timeline_sept_2021_descriptor.png"),
       dpi = 600)

