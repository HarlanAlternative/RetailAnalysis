# 测试ARIMA修复的简化脚本
library(tidyverse)
library(arrow)
library(forecast)

# 加载数据
panel <- read_parquet("data_processed/panel.parquet")

# 设置时间切分
train_weeks <- 1:320
test_weeks <- 321:399

# 选择几个门店进行测试
test_stores <- c(100, 101, 102, 103, 104)
test_data <- panel %>% filter(store %in% test_stores)

cat("Testing ARIMA fixes on", length(test_stores), "stores...\n")

# 定义ARIMA训练函数 - 先测试纯时间序列
fit_predict_store <- function(df_train, df_test) {
  y_tr <- ts(df_train$units, frequency = 52)
  
  if (length(y_tr) < 80) stop("too_few_points")
  
  # 先尝试纯时间序列auto.arima
  tryCatch({
    fit <- forecast::auto.arima(y_tr, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
    fc <- forecast::forecast(fit, h = nrow(df_test))
    return(list(pred = as.numeric(fc$mean), method = "auto.arima_pure"))
  }, error = function(e1) {
    # 如果纯时间序列失败，尝试带外生变量
    tryCatch({
      x_tr <- as.matrix(df_train[, c("avg_price", "promo_share", "price_change")])
      x_te <- as.matrix(df_test[, c("avg_price", "promo_share", "price_change")])
      
      # 检查外生变量是否有NA
      if(any(is.na(x_tr)) || any(is.na(x_te))) {
        stop("NA_in_xreg")
      }
      
      fit <- forecast::auto.arima(y_tr, xreg = x_tr, seasonal = TRUE,
                                  stepwise = TRUE, approximation = FALSE)
      fc <- forecast::forecast(fit, xreg = x_te, h = nrow(df_test))
      return(list(pred = as.numeric(fc$mean), method = "auto.arima_xreg"))
    }, error = function(e2) {
      # 最后回退到季节朴素
      fc <- forecast::snaive(y_tr, h = nrow(df_test))
      return(list(pred = as.numeric(fc$mean), method = "snaive"))
    })
  })
}

# 测试每个门店
results <- list()
for(store_id in test_stores) {
  cat("Testing store", store_id, "...\n")
  
  store_data <- test_data %>% filter(store == store_id)
  tr <- store_data %>% filter(week %in% train_weeks)
  te <- store_data %>% filter(week %in% test_weeks)
  
  if(nrow(tr) > 0 && nrow(te) > 0) {
    tryCatch({
      result <- fit_predict_store(tr, te)
      results[[as.character(store_id)]] <- list(
        store = store_id,
        method = result$method,
        success = TRUE,
        n_train = nrow(tr),
        n_test = nrow(te)
      )
      cat("  Success:", result$method, "\n")
    }, error = function(e) {
      results[[as.character(store_id)]] <- list(
        store = store_id,
        method = "failed",
        success = FALSE,
        error = e$message,
        n_train = nrow(tr),
        n_test = nrow(te)
      )
      cat("  Failed:", e$message, "\n")
    })
  } else {
    cat("  Insufficient data\n")
  }
}

# 总结结果
cat("\n=== ARIMA Test Results ===\n")
for(result in results) {
  cat("Store", result$store, ":", result$method, 
      if(result$success) "(Success)" else "(Failed)", "\n")
}

# 统计方法分布
methods <- sapply(results, function(x) x$method)
method_table <- table(methods)
cat("\nMethod distribution:\n")
print(method_table)
