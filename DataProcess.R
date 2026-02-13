############################################
## Milestone 1 — 数据处理 & 数据探索（最简版）
## 只做评分细则要求 & 与参考 PDF 对齐；不做多余内容
############################################

############################################
## 0) 准备：安装/加载必要包
############################################
need_pkgs <- c("tidyverse","zoo","forecast","ggplot2")
to_install <- need_pkgs[!need_pkgs %in% installed.packages()[,1]]
if(length(to_install) > 0){
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(zoo)
library(forecast)
library(ggplot2)

############################################
## 1) 小工具：读取 CSV（当前目录或 /mnt/data）
############################################
read_csv_safely <- function(name){
  if(file.exists(name)) {
    readr::read_csv(name, show_col_types = FALSE)
  } else if(file.exists(file.path("/mnt/data", name))) {
    readr::read_csv(file.path("/mnt/data", name), show_col_types = FALSE)
  } else {
    stop(paste0("找不到文件：", name, "（当前目录或 /mnt/data）"))
  }
}

############################################
## 2) 读取数据：wana / ccount / demo 及可选 ILI
############################################
wana   <- read_csv_safely("wana.csv")
ccount <- read_csv_safely("ccount.csv")
demo   <- read_csv_safely("demo.csv")

ili_ok <- FALSE
if(file.exists("ili.csv") || file.exists("/mnt/data/ili.csv")){
  ili_raw <- if(file.exists("ili.csv")) readr::read_csv("ili.csv", show_col_types = FALSE)
  else readr::read_csv("/mnt/data/ili.csv", show_col_types = FALSE)
  # 猜列名（WEEK/week/Week/wk, ILI/ili/weighted_ili/wili/ilitotal）
  wk_cand  <- c("WEEK","week","Week","wk","week_number")
  ili_cand <- c("ILI","ili","weighted_ili","wili","ilitotal")
  wk_col   <- wk_cand[wk_cand %in% names(ili_raw)]
  ili_col  <- ili_cand[ili_cand %in% names(ili_raw)]
  if(length(wk_col) > 0 && length(ili_col) > 0){
    ili_clean <- tibble(
      week_number = suppressWarnings(as.integer(ili_raw[[wk_col[1]]])),
      ili         = suppressWarnings(as.numeric(ili_raw[[ili_col[1]]]))
    ) %>%
      filter(!is.na(week_number)) %>%
      group_by(week_number) %>%
      summarise(ili = mean(ili, na.rm = TRUE), .groups = "drop") %>%
      arrange(week_number)
    # 小缺口插值（最多 2 周），首尾前后填充
    ili_clean$ili <- zoo::na.approx(ili_clean$ili, maxgap = 2, na.rm = FALSE)
    ili_clean$ili <- zoo::na.locf(ili_clean$ili, na.rm = FALSE)
    ili_clean$ili <- zoo::na.locf(ili_clean$ili, na.rm = FALSE, fromLast = TRUE)
    ili_ok <- TRUE
  }
}

############################################
## 3) 数据处理（评分项：说明“如何 & 为什么” + 前后对比）
############################################
## 3.1 清洗前的简单概览（用于“前”对比）
summary_before <- wana %>%
  summarise(
    n_rows = n(),
    sale_na_rate = mean(is.na(SALE)),
    move_neg = sum(MOVE < 0, na.rm = TRUE),
    price_neg = sum(PRICE < 0, na.rm = TRUE),
    price_p1 = quantile(PRICE, 0.01, na.rm = TRUE),
    price_p99 = quantile(PRICE, 0.99, na.rm = TRUE)
  )
readr::write_csv(summary_before, "summary_before_wana.csv")

## 3.2 清洗规则：
## - SALE 缺失→0（无促销）：缺失说明未记录促销，按“未促销”处理，避免把 NA 当噪声
## - 去掉 MOVE/PRICE 负值：物理不合理
## - PRICE winsorize 到 [P1,P99]：抑制极端值对均值的影响，提升稳健性
wana1 <- wana %>%
  mutate(SALE = ifelse(is.na(SALE), 0, 1)) %>%
  filter(MOVE >= 0, PRICE >= 0)

p1 <- quantile(wana1$PRICE, 0.01, na.rm = TRUE)
p99 <- quantile(wana1$PRICE, 0.99, na.rm = TRUE)
wana1 <- wana1 %>% mutate(PRICE = pmin(pmax(PRICE, p1), p99))

## 3.3 聚合到“店-周”级：销量总和 / 价格均值 / 促销取 max
wana_weekly <- wana1 %>%
  group_by(STORE, WEEK) %>%
  summarise(
    total_sales = sum(MOVE,  na.rm = TRUE),
    avg_price   = mean(PRICE, na.rm = TRUE),
    promo_flag  = max(SALE,   na.rm = TRUE),
    .groups = "drop"
  )

## 3.4 清洗后的简要概览（用于“后”对比）
summary_after <- wana1 %>%
  summarise(
    n_rows = n(),
    sale_na_rate = mean(is.na(SALE)),
    move_neg = sum(MOVE < 0, na.rm = TRUE),
    price_neg = sum(PRICE < 0, na.rm = TRUE),
    price_p1 = quantile(PRICE, 0.01, na.rm = TRUE),
    price_p99 = quantile(PRICE, 0.99, na.rm = TRUE)
  )
readr::write_csv(summary_after, "summary_after_wana.csv")

## 3.5 ccount：仅保留需要的列并去缺失
ccount_clean <- ccount %>%
  select(store, week, custcoun) %>%
  filter(!is.na(week), !is.na(custcoun), custcoun >= 0)

## 3.6 demo：精简关键字段（用于控制/分层）
demo_clean <- demo %>%
  select(store, city, zip, scluster, zone, weekvol, pricmed)

## 3.7 合并到店-周面板
final_data <- wana_weekly %>%
  left_join(ccount_clean, by = c("STORE" = "store", "WEEK" = "week")) %>%
  left_join(demo_clean,   by = c("STORE" = "store"))

if(isTRUE(ili_ok)){
  final_data <- final_data %>% left_join(ili_clean, by = c("WEEK" = "week_number"))
} else {
  final_data <- final_data %>% mutate(ili = NA_real_)
}

readr::write_csv(final_data, "final_store_week_panel.csv")

############################################
## 4) 数据探索（严格按：折线 / STL / 滞后相关 三项）
############################################
## 4.1 准备按周聚合（总销量，ILI 均值）
trend_df <- final_data %>%
  group_by(WEEK) %>%
  summarise(
    sales = sum(total_sales, na.rm = TRUE),
    ili   = mean(ili, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(WEEK)

## 4.2 折线图：销售 vs. ILI（标准化到同一尺度，便于肉眼比较）
trend_std <- trend_df %>%
  mutate(
    sales_z = as.numeric(scale(sales)),
    ili_z   = if(!all(is.na(ili))) as.numeric(scale(ili)) else NA_real_
  )

p_trend <- ggplot(trend_std, aes(x = WEEK)) +
  geom_line(aes(y = sales_z, color = "Sales (z)")) +
  { if(isTRUE(ili_ok)) geom_line(aes(y = ili_z, color = "ILI (z)")) } +
  labs(title = "Weekly Sales vs. ILI (standardized)",
       x = "Week", y = "Z-score") +
  scale_color_manual(values = c("Sales (z)" = "#1f77b4", "ILI (z)" = "#ff7f0e")) +
  theme_minimal()
ggsave("plot_trend_sales_vs_ili.png", p_trend, width = 8, height = 4, dpi = 150)

## 4.3 STL 分解：销售 & ILI（若有）
## - stl(s.window="periodic") 需 ≥ ~104 周；否则用整数窗口（如 13）
enough <- nrow(trend_df) >= 104
swin <- if(enough) "periodic" else 13

sales_ts <- ts(forecast::na.interp(trend_df$sales), frequency = 52)
sales_stl <- stl(sales_ts, s.window = swin)
png("plot_stl_sales.png", width = 900, height = 600)
plot(sales_stl, main = sprintf("STL Decomposition - Sales (s.window=%s)", swin))
dev.off()

if(isTRUE(ili_ok) && !all(is.na(trend_df$ili))){
  ili_ts <- ts(forecast::na.interp(trend_df$ili), frequency = 52)
  ili_stl <- stl(ili_ts, s.window = swin)
  png("plot_stl_ili.png", width = 900, height = 600)
  plot(ili_stl, main = sprintf("STL Decomposition - ILI (s.window=%s)", swin))
  dev.off()
}

## 4.4 滞后相关：lag 0–4（Pearson）
lags <- 0:4
corr_vals <- rep(NA_real_, length(lags))

if(isTRUE(ili_ok) && !all(is.na(trend_df$ili))){
  for(i in seq_along(lags)){
    L <- lags[i]
    ili_lag <- dplyr::lag(trend_df$ili, n = L)
    ili_lag <- zoo::na.approx(ili_lag, na.rm = FALSE)
    ili_lag <- zoo::na.locf(ili_lag, na.rm = FALSE)
    ili_lag <- zoo::na.locf(ili_lag, na.rm = FALSE, fromLast = TRUE)
    ok <- complete.cases(ili_lag, trend_df$sales)
    if(sum(ok) > 2){
      corr_vals[i] <- cor(ili_lag[ok], trend_df$sales[ok])
    }
  }
}

corr_tbl <- tibble(lag = lags, correlation = corr_vals)
readr::write_csv(corr_tbl, "lag_correlation_summary.csv")

p_corr <- ggplot(corr_tbl, aes(x = factor(lag), y = correlation)) +
  geom_col() +
  labs(title = "Pearson Correlation (Sales vs. ILI lagged 0–4)",
       x = "Lag (weeks)", y = "Correlation") +
  theme_minimal()
ggsave("plot_correlation_lag.png", p_corr, width = 6, height = 4, dpi = 150)

############################################
## 5) 最小化导出清单（正文用图 & 前后对比表）
############################################
# 图：折线 / STL(销售 & ILI*) / 滞后相关
# 表：summary_before_wana.csv, summary_after_wana.csv, lag_correlation_summary.csv, final_store_week_panel.csv
cat("已导出文件：\n",
    "  - plot_trend_sales_vs_ili.png\n",
    "  - plot_stl_sales.png\n",
    if(isTRUE(ili_ok)) "  - plot_stl_ili.png\n" else "",
    "  - plot_correlation_lag.png\n",
    "  - summary_before_wana.csv\n",
    "  - summary_after_wana.csv\n",
    "  - lag_correlation_summary.csv\n",
    "  - final_store_week_panel.csv\n", sep = "")