source(here("src", "util", "data_util.R"))

## Load list of descriptors that could plausibly be related to flooding
## Others are either internal use or clearly unrelated
sewer_descriptors <-
  read_csv(here("data",
                "interim",
                "unique_sewer_descriptors.csv")) %>%
  filter(flood_related) %>%
  pull(descriptor)

df_sewer_calls <-
  load_sewer_calls() %>%
  filter(descriptor %in% sewer_descriptors)

df_sewer_calls_daily <- 
  df_sewer_calls %>%
  group_by(created_date, descriptor) %>%
  summarise(n_calls = n(),
            desc_daily_calls = sum(n_calls),
            created_date = first(created_date),
            precip_in = first(precip_in),
            descriptor_short = first(descriptor_plot_label),
            descriptor_plot_label = first(descriptor_plot_label)) %>%
  ungroup() %>%
  group_by(created_date) %>%
  mutate(total_daily_calls = sum(n_calls)) %>%
  ungroup()

## Plot number of calls by precipitation for each call descriptor
df_sewer_calls_daily %>%
  ggplot(aes(x = precip_in, y = n_calls, color = descriptor)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Number of sewer calls by precipitation",
       x = "Precipitation (in)",
       y = "Number of calls") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Plot bar chart of total calls by descriptor
p_descriptor_bar_chart <-
  df_sewer_calls %>%
  group_by(descriptor_plot_label) %>%
  summarise(total_calls_with_desc = n()) %>%
  mutate(descriptor_plot_label = fct_reorder(descriptor_plot_label,
                                             total_calls_with_desc,
                                             .desc = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = descriptor_plot_label, y = total_calls_with_desc)) +
  labs(title = "Total number of calls by call type (2010–2024)",
       x = "Call type",
       y = "Number of calls") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## Plot relationship between precipitation and calls by descriptor
 p_scatter_by_descriptor <- 
   df_sewer_calls_daily %>%
   ggplot(aes(x = precip_in,
              y = n_calls)) +
   geom_point(alpha = .05) +
   ## geom_smooth(method = "loess") +
   labs(title = "Daily number of 311 calls by daily precipitation",
        x = "Precipitation (in)",
        y = "Number of calls") +
   theme_bw() +
   theme(legend.position = "bottom") +
   scale_x_log10() +
   scale_y_log10() +
   facet_wrap(~str_wrap(descriptor_plot_label, 15))

p_combined <- plot_grid(p_descriptor_bar_chart, p_scatter_by_descriptor, ncol = 2)

ggsave(here("reports",
            "figures",
            "call_descriptor_precip_response.png"),
       p_combined,
       width = 10,
       height = 5,
       dpi = 600)

## Facet and log
df_sewer_calls_daily %>%
  ggplot(aes(x = precip_in,
             y = n_calls)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "loess") +
  labs(title = "Percentage of sewer calls by precipitation",
       x = "Precipitation (in)",
       y = "Percentage of calls") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~descriptor, scales = "free_y") +
  scale_x_log10()

ggsave(here("reports",
            "figures",
            "call_descriptor_precip_response_log.png"),
       p_descriptors,
       width = 10,
       height = 10,
       dpi = 600)

## Facet and log, at least 1 inch of rain
df_sewer_calls_daily %>%
  filter(total_calls_with_desc > 1000,
         precip_in > 1) %>%
  ggplot(aes(x = precip_in,
             y = n_calls)) +
  geom_point(aes(color = created_date)) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess",
              color = "red") +
  labs(title = "Percentage of sewer calls by precipitation",
       x = "Precipitation (in)",
       y = "Percentage of calls") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~descriptor, scales = "free_y") +
  ## scale_x_log10() +
  scale_color_viridis_c()
  geom_text(aes(label = created_date), nudge_y = 1)

## Select descriptors that are responsive to precipitation
precip_descriptors <-
  c("Catch Basin Clogged/Flooding (Use Comments) (SC)",
    "Street Flooding (SJ)",
    "Sewer Backup (Use Comments) (SA)")

## ## Further analyses to check if all of these descriptors should be included
## ## Pie chart of calls by descriptor
## df_sewer_calls_daily %>%
##   filter(descriptor %in% precip_descriptors) %>%
##   ggplot(aes(x = 1, y = n_calls, fill = descriptor_plot_label)) +
##   geom_bar(stat = "identity") +
##   coord_polar("y") +
##   theme_minimal() +
##   theme(legend.position = "bottom") +
##   ## no axes
##   theme(axis.text.x = element_blank(),
##         axis.text.y = element_blank(),
##         axis.ticks = element_blank(),
##         axis.title.x = element_blank(),
##         axis.title.y = element_blank(),
##         legend.direction = "vertical") +
##   scale_fill_brewer(palette = "Set1") 

# Pivot the data wider
df_wide <- df_311_daily %>%
  pivot_wider(names_from = descriptor, values_from = n_calls, values_fill = 0)

# Create the correlation matrix
corr_matrix <- cor(df_wide[, 3:ncol(df_wide)])

# Print the correlation matrix
print(corr_matrix)

# Create the scatterplot matrix plot
p_descriptor_scatterplot_matrix <-
  pairs(df_wide[, 3:ncol(df_wide)], main = "Scatterplot Matrix of n_calls by Descriptor Type",
        pch = 19, cex = 0.5, col = "blue", lower.panel = NULL)
