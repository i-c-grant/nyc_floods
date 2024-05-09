library(dplyr)
library(tidyr)

df_311_daily <-
  read_csv(here("data",
                "processed",
                "311",
                "precip_responsive_calls_daily.csv"))

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

png(here("reports", "figures", "scatterplot_matrix.png"),
    width = 1000,
    height = 1000)
pairs(df_wide[, 3:ncol(df_wide)], main = "Scatterplot Matrix of n_calls by Descriptor Type",
      pch = 19, cex = 0.5, col = "blue", lower.panel = NULL)
dev.off()

