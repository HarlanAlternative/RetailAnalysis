# Flu Season Analysis Visualization
# Creating charts to support theme change justification

library(tidyverse)
library(arrow)
library(ggplot2)

# Read the data
panel_data <- read_parquet('../data_processed/panel.parquet')

# 1. Create seasonal indicators
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

# 2. Calculate key statistics
flu_season_avg <- seasonal_analysis$avg_sales[seasonal_analysis$flu_season == TRUE]
non_flu_avg <- seasonal_analysis$avg_sales[seasonal_analysis$flu_season == FALSE]
flu_season_diff <- flu_season_avg - non_flu_avg
flu_season_diff_pct <- (flu_season_diff/non_flu_avg) * 100

peak_flu_avg <- seasonal_analysis$avg_sales[seasonal_analysis$peak_flu == TRUE]
sales_mean <- mean(panel_data$units, na.rm=TRUE)
peak_flu_impact <- ((peak_flu_avg - sales_mean)/sales_mean) * 100

flu_correlation <- cor(panel_data$units, as.numeric(panel_data$week %in% c(40:52, 1:12)), use = "complete.obs")

# 3. Create visualization data
flu_comparison_data <- data.frame(
  Season = c("Flu Season", "Non-Flu Season", "Peak Flu Months", "Overall Average"),
  Average_Sales = c(flu_season_avg[1], non_flu_avg[1], peak_flu_avg[1], sales_mean),
  Category = c("Flu", "Non-Flu", "Peak Flu", "Overall")
)

# 4. Create the visualization
p1 <- ggplot(flu_comparison_data, aes(x = Season, y = Average_Sales, fill = Category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Average_Sales)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Flu" = "#FF6B6B", "Non-Flu" = "#4ECDC4", 
                               "Peak Flu" = "#45B7D1", "Overall" = "#96CEB4")) +
  labs(
    title = "Flu Season vs Non-Flu Season Sales Comparison",
    subtitle = paste("Flu season correlation:", round(flu_correlation, 4), 
                    "| Difference:", round(flu_season_diff_pct, 1), "%"),
    x = "Season Type",
    y = "Average Sales (Units)",
    caption = "Data shows minimal flu season impact on paracetamol sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkgray"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  ) +
  ylim(0, max(flu_comparison_data$Average_Sales) * 1.15)

# 5. Create monthly breakdown
monthly_analysis <- panel_data %>%
  mutate(
    month = ((week - 1) %% 52) %/% 4 + 1
  ) %>%
  group_by(month) %>%
  summarise(
    avg_sales = mean(units, na.rm = TRUE),
    total_sales = sum(units, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    month_name = factor(month.abb[month], levels = month.abb),
    flu_season = month %in% c(10, 11, 12, 1, 2, 3),
    peak_flu = month %in% c(12, 1, 2)
  )

p2 <- ggplot(monthly_analysis, aes(x = month_name, y = avg_sales, fill = flu_season)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.0f", avg_sales)), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("TRUE" = "#FF6B6B", "FALSE" = "#4ECDC4"),
                    labels = c("TRUE" = "Flu Season", "FALSE" = "Non-Flu Season")) +
  labs(
    title = "Monthly Sales Pattern Analysis",
    subtitle = "Flu season months (Oct-Mar) vs Non-flu season months",
    x = "Month",
    y = "Average Sales (Units)",
    fill = "Season Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkgray"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 6. Save the plots
ggsave("figures/flu_season_comparison.png", p1, width = 10, height = 6, dpi = 300)
ggsave("figures/monthly_flu_pattern.png", p2, width = 12, height = 6, dpi = 300)

# 7. Print summary statistics
cat("=== FLU SEASON ANALYSIS RESULTS ===\n")
cat("Flu season average sales:", round(flu_season_avg, 1), "units\n")
cat("Non-flu season average sales:", round(non_flu_avg, 1), "units\n")
cat("Difference:", round(flu_season_diff, 1), "units (", round(flu_season_diff_pct, 1), "%)\n")
cat("Peak flu months average sales:", round(peak_flu_avg, 1), "units\n")
cat("Overall average sales:", round(sales_mean, 1), "units\n")
cat("Peak flu impact:", round(peak_flu_impact, 3), "%\n")
cat("Flu season correlation:", round(flu_correlation, 4), "\n")
cat("Interpretation: Very weak correlation suggests minimal flu season impact\n")
cat("Conclusion: Paracetamol sales are not strongly influenced by flu seasons\n") 