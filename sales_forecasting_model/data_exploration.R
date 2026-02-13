setwd("~/Desktop/for765/project/newTryM2/sales_forecasting_model")
# Load required packages
library(tidyverse)
library(arrow)
library(ggplot2)


# Read the data
panel_data <- read_parquet('../data_processed/panel.parquet')

# 1. Sales Analysis
# Basic statistics
units_stats <- summary(panel_data$units)
sales_mean <- mean(panel_data$units, na.rm=TRUE)
sales_median <- units_stats[3]
sales_sd <- sd(panel_data$units, na.rm=TRUE)
sales_cv <- sales_sd/sales_mean

# Store-level analysis
store_performance <- panel_data %>%
  group_by(store) %>%
  summarise(
    total_sales = sum(units, na.rm = TRUE),
    avg_weekly_sales = mean(units, na.rm = TRUE),
    sales_volatility = sd(units, na.rm = TRUE),
    .groups = 'drop'
  )

avg_store_sales <- mean(store_performance$total_sales)
avg_weekly_sales <- mean(store_performance$avg_weekly_sales)

# 2. Temporal Patterns
# Weekly sales trends
weekly_trends <- panel_data %>%
  group_by(week) %>%
  summarise(
    total_units = sum(units, na.rm = TRUE),
    avg_units = mean(units, na.rm = TRUE),
    store_count = n_distinct(store),
    .groups = 'drop'
  )

avg_weekly_total <- mean(weekly_trends$total_units)
sales_range_min <- min(weekly_trends$total_units)
sales_range_max <- max(weekly_trends$total_units)

# 3. Feature Analysis
# Available features
available_features <- colnames(panel_data)
total_features <- length(available_features)

# Key features for analysis
key_features <- c("units", "avg_price", "income", "age9", "age60", "educ", "density", "unemp")
available_key_features <- intersect(key_features, available_features)

# 4. Correlation Analysis
# Calculate correlations with sales
correlations <- data.frame(
  feature = available_key_features,
  correlation = sapply(available_key_features, function(x) {
    tryCatch({
      cor(panel_data$units, as.numeric(panel_data[[x]]), use = "complete.obs")
    }, error = function(e) NA)
  })
)

# Sort correlations
correlations <- correlations %>%
  arrange(desc(abs(correlation))) %>%
  filter(!is.na(correlation))

# 5. Store Clustering
# Cluster stores by performance
store_clusters <- store_performance %>%
  mutate(
    performance_cluster = case_when(
      avg_weekly_sales > quantile(avg_weekly_sales, 0.75, na.rm = TRUE) ~ "High Performance",
      avg_weekly_sales > quantile(avg_weekly_sales, 0.25, na.rm = TRUE) ~ "Medium Performance", 
      TRUE ~ "Low Performance"
    )
  )

# Cluster statistics
cluster_summary <- store_clusters %>%
  group_by(performance_cluster) %>%
  summarise(
    store_count = n(),
    avg_sales = mean(avg_weekly_sales),
    .groups = 'drop'
  )

# 6. Flu Season Analysis (Supporting Theme Change)
# Create seasonal indicators
seasonal_analysis <- panel_data %>%
  mutate(
    month = ((week - 1) %% 52) %/% 4 + 1,  # Approximate month from week
    flu_season = month %in% c(10, 11, 12, 1, 2, 3),  # Oct-Mar flu season
    peak_flu = month %in% c(12, 1, 2)  # Dec-Feb peak flu months
  ) %>%
  group_by(flu_season, peak_flu) %>%
  summarise(
    avg_sales = mean(units, na.rm = TRUE),
    sales_count = n(),
    .groups = 'drop'
  )

# Flu season comparison
flu_season_avg <- seasonal_analysis$avg_sales[seasonal_analysis$flu_season == TRUE]
non_flu_avg <- seasonal_analysis$avg_sales[seasonal_analysis$flu_season == FALSE]
flu_season_diff <- flu_season_avg - non_flu_avg
flu_season_diff_pct <- (flu_season_diff/non_flu_avg) * 100

# Peak flu months analysis
peak_flu_avg <- seasonal_analysis$avg_sales[seasonal_analysis$peak_flu == TRUE]
peak_flu_impact <- ((peak_flu_avg - sales_mean)/sales_mean) * 100

# Flu season correlation
flu_correlation <- cor(panel_data$units, as.numeric(panel_data$week %in% c(40:52, 1:12)), use = "complete.obs")

# 7. Basic Visualizations for Report
# Create output directory for figures
dir.create("figures", showWarnings = FALSE)

# Weekly sales trend plot
p1 <- ggplot(weekly_trends, aes(x = week, y = total_units)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", alpha = 0.3) +
  labs(
    title = "Weekly Sales Trends (399 Weeks)",
    subtitle = "Clear temporal patterns support time series modeling",
    x = "Week Number",
    y = "Total Sales Units"
  ) +
  theme_minimal()

ggsave("figures/weekly_sales_trends.png", p1, width = 10, height = 6, dpi = 300)

# Store performance clustering plot
p2 <- ggplot(store_clusters, aes(x = avg_weekly_sales, y = sales_volatility, color = performance_cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("High Performance" = "green", "Medium Performance" = "orange", "Low Performance" = "red")) +
  labs(
    title = "Store Performance Clustering",
    subtitle = "Store classification based on sales performance and volatility",
    x = "Average Weekly Sales (Units)",
    y = "Sales Volatility (Standard Deviation)",
    color = "Performance Cluster"
  ) +
  theme_minimal()

ggsave("figures/store_clustering.png", p2, width = 10, height = 6, dpi = 300)

# Correlation heatmap
correlation_matrix <- correlations %>%
  pivot_wider(names_from = feature, values_from = correlation) %>%
  as.matrix()

png("figures/correlation_heatmap.png", width = 8, height = 6, units = "in", res = 300)
corrplot::corrplot(correlation_matrix, 
         method = "color",
         type = "upper",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         title = "Feature Correlation with Sales",
         mar = c(0,0,2,0))
dev.off()

# Display key results
cat("Analysis Complete. Key results available in variables above.\n")
cat("Figures saved in 'figures/' directory for report inclusion.\n") 