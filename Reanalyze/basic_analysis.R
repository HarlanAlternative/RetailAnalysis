############################################
## Milestone 3 - Basic Sales Forecasting Analysis
## 使用基础R包进行建模分析
############################################

# 设置工作目录
setwd("C:/Users/Administrator/Desktop/project/newTryM3")

# 创建输出目录
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

############################################
## 1. 数据加载和预处理
############################################
cat("Loading data...\n")

# 读取CSV数据（如果parquet不可用）
if(file.exists('data_processed/panel.parquet')) {
  # 尝试使用arrow包
  if(require(arrow, quietly = TRUE)) {
    panel_data <- read_parquet('data_processed/panel.parquet')
  } else {
    # 如果没有arrow包，尝试读取CSV
    panel_data <- read.csv('../forOtherFactors/final_store_week_panel.csv')
    # 重命名列以匹配我们的代码
    colnames(panel_data) <- gsub("total_sales", "units", colnames(panel_data))
    colnames(panel_data) <- gsub("STORE", "store", colnames(panel_data))
    colnames(panel_data) <- gsub("WEEK", "week", colnames(panel_data))
  }
} else {
  # 读取CSV文件
  panel_data <- read.csv('../forOtherFactors/final_store_week_panel.csv')
  colnames(panel_data) <- gsub("total_sales", "units", colnames(panel_data))
  colnames(panel_data) <- gsub("STORE", "store", colnames(panel_data))
  colnames(panel_data) <- gsub("WEEK", "week", colnames(panel_data))
}

# 基本数据概览
cat("Data dimensions:", dim(panel_data), "\n")
cat("Variables:", colnames(panel_data), "\n")

# 数据质量检查
summary_stats <- list(
  total_observations = nrow(panel_data),
  unique_stores = length(unique(panel_data$store)),
  unique_weeks = length(unique(panel_data$week)),
  missing_units = sum(is.na(panel_data$units)),
  units_mean = mean(panel_data$units, na.rm = TRUE),
  units_sd = sd(panel_data$units, na.rm = TRUE)
)

print(summary_stats)

############################################
## 2. 基础特征工程
############################################
cat("Performing basic feature engineering...\n")

# 创建时间特征
featured_data <- panel_data
featured_data$year <- floor(featured_data$week / 52) + 2005
featured_data$month <- ((featured_data$week - 1) %% 52) %/% 4 + 1
featured_data$quarter <- ceiling(featured_data$month / 3)
featured_data$is_holiday_season <- featured_data$month %in% c(11, 12, 1, 2)

# 排序数据以创建滞后特征
featured_data <- featured_data[order(featured_data$store, featured_data$week), ]

# 创建滞后特征（按商店分组）
stores <- unique(featured_data$store)
for(store_id in stores) {
  store_mask <- featured_data$store == store_id
  store_data <- featured_data[store_mask, ]
  
  if(nrow(store_data) > 0) {
    # 滞后特征
    store_data$units_lag1 <- c(NA, store_data$units[-nrow(store_data)])
    store_data$units_lag2 <- c(NA, NA, store_data$units[-(nrow(store_data):(nrow(store_data)-1))])
    
    # 简单的移动平均
    store_data$units_ma4 <- sapply(1:nrow(store_data), function(i) {
      start_idx <- max(1, i-3)
      mean(store_data$units[start_idx:i], na.rm = TRUE)
    })
    
    featured_data[store_mask, ] <- store_data
  }
}

# 处理缺失值
featured_data$units_lag1[is.na(featured_data$units_lag1)] <- 0
featured_data$units_lag2[is.na(featured_data$units_lag2)] <- 0

cat("Feature engineering completed. New dimensions:", dim(featured_data), "\n")

############################################
## 3. 数据分割
############################################
cat("Preparing data for modeling...\n")

# 移除缺失值
model_data <- featured_data[!is.na(featured_data$units) & !is.na(featured_data$avg_price), ]

# 选择关键特征
key_features <- c("units", "avg_price", "units_lag1", "units_lag2", "units_ma4",
                  "month", "quarter", "is_holiday_season")

# 确保所有特征都存在
available_features <- intersect(key_features, colnames(model_data))
model_subset <- model_data[, available_features]

cat("Selected features for modeling:", available_features, "\n")

# 数据分割（时间序列分割）
train_size <- floor(0.8 * nrow(model_subset))
train_data <- model_subset[1:train_size, ]
test_data <- model_subset[(train_size + 1):nrow(model_subset), ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

############################################
## 4. 线性回归模型
############################################
cat("Training Linear Regression model...\n")

# 准备训练数据
train_features <- setdiff(available_features, "units")
train_formula <- as.formula(paste("units ~", paste(train_features, collapse = " + ")))

# 训练线性回归
lm_model <- lm(train_formula, data = train_data)

# 预测
lm_predictions <- predict(lm_model, test_data)

# 评估
lm_rmse <- sqrt(mean((test_data$units - lm_predictions)^2, na.rm = TRUE))
lm_mae <- mean(abs(test_data$units - lm_predictions), na.rm = TRUE)
lm_mape <- mean(abs((test_data$units - lm_predictions) / test_data$units), na.rm = TRUE) * 100

cat("Linear Regression Results:\n")
cat("RMSE:", round(lm_rmse, 2), "\n")
cat("MAE:", round(lm_mae, 2), "\n")
cat("MAPE:", round(lm_mape, 2), "%\n")

# 模型摘要
print("Linear Regression Model Summary:")
print(summary(lm_model))

############################################
## 5. 简单时间序列分析
############################################
cat("Training simple time series model...\n")

# 准备时间序列数据（使用总体销售数据）
ts_data <- aggregate(units ~ week, data = model_data, FUN = sum)
ts_data <- ts_data[order(ts_data$week), ]
ts_values <- ts_data$units

# 转换为时间序列
ts_object <- ts(ts_values, frequency = 52)

# 分割时间序列数据
ts_train_size <- floor(0.8 * length(ts_object))
ts_train <- ts_object[1:ts_train_size]
ts_test <- ts_object[(ts_train_size + 1):length(ts_object)]

# 简单的移动平均预测
if(length(ts_test) > 0) {
  # 使用最后12个值的平均作为预测
  window_size <- min(12, length(ts_train))
  ts_predictions <- rep(mean(tail(ts_train, window_size)), length(ts_test))
  
  # 评估
  ts_rmse <- sqrt(mean((ts_test - ts_predictions)^2, na.rm = TRUE))
  ts_mae <- mean(abs(ts_test - ts_predictions), na.rm = TRUE)
  ts_mape <- mean(abs((ts_test - ts_predictions) / ts_test), na.rm = TRUE) * 100
  
  cat("Time Series (Moving Average) Results:\n")
  cat("RMSE:", round(ts_rmse, 2), "\n")
  cat("MAE:", round(ts_mae, 2), "\n")
  cat("MAPE:", round(ts_mape, 2), "%\n")
} else {
  ts_rmse <- ts_mae <- ts_mape <- NA
}

############################################
## 6. 结果可视化（基础R）
############################################
cat("Creating visualizations...\n")

# 模型性能对比
performance_comparison <- data.frame(
  Model = c("Linear Regression", "Time Series MA"),
  RMSE = c(lm_rmse, ts_rmse),
  MAE = c(lm_mae, ts_mae),
  MAPE = c(lm_mape, ts_mape)
)

print("Model Performance Comparison:")
print(performance_comparison)

# 预测vs实际值对比图（线性回归）
png("figures/lm_predictions_vs_actual.png", width = 800, height = 600)
plot(test_data$units, lm_predictions, 
     main = "Linear Regression: Predicted vs Actual Sales",
     xlab = "Actual Units", ylab = "Predicted Units",
     pch = 16, col = "steelblue", alpha = 0.6)
abline(0, 1, col = "red", lty = 2)
text(x = max(test_data$units) * 0.1, y = max(lm_predictions) * 0.9,
     labels = paste("RMSE:", round(lm_rmse, 2), "| MAE:", round(lm_mae, 2)))
dev.off()

# 时间序列图
if(!is.na(ts_rmse)) {
  png("figures/time_series_analysis.png", width = 1000, height = 600)
  plot(ts_object, main = "Weekly Total Sales Time Series", 
       xlab = "Time", ylab = "Total Units")
  abline(v = ts_train_size/length(ts_object), col = "red", lty = 2)
  legend("topright", legend = c("Training", "Test"), col = c("black", "red"), lty = c(1, 2))
  dev.off()
}

############################################
## 7. 保存结果
############################################
cat("Saving results...\n")

# 保存模型性能
write.csv(performance_comparison, "results/model_performance.csv", row.names = FALSE)

# 保存预测结果
predictions_df <- data.frame(
  week = test_data$week,
  actual = test_data$units,
  lm_predicted = lm_predictions
)

write.csv(predictions_df, "results/model_predictions.csv", row.names = FALSE)

# 保存模型摘要
capture.output(summary(lm_model), file = "results/lm_model_summary.txt")

cat("Analysis completed successfully!\n")
cat("Results saved in 'results/' directory\n")
cat("Figures saved in 'figures/' directory\n")

# 输出最终总结
cat("\n=== FINAL RESULTS SUMMARY ===\n")
print(performance_comparison)
