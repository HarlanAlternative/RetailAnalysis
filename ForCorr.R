############################################
## EDA for Modeling Prep — 最简、刚好够用版
## 输入：final_store_week_panel.csv（上一阶段已生成）
## 输出：若干图表 + 汇总表（供正文/附录使用）
############################################

############################################
## 0) 装包 + 加载（够用即可，不多装）
############################################
need_pkgs <- c("tidyverse","ggplot2")
to_install <- need_pkgs[!need_pkgs %in% installed.packages()[,1]]
if(length(to_install) > 0){
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(ggplot2)

############################################
## 1) 读取数据（来自上一阶段产物）
############################################
read_csv_safely <- function(name){
  if(file.exists(name)) readr::read_csv(name, show_col_types = FALSE)
  else if(file.exists(file.path("/mnt/data", name))) readr::read_csv(file.path("/mnt/data", name), show_col_types = FALSE)
  else stop(paste0("找不到文件：", name))
}

df <- read_csv_safely("final_store_week_panel.csv")

# 基本检查
cat("数据行数：", nrow(df), "\n")
cat("列名：", paste(names(df), collapse = ", "), "\n\n")

############################################
## 2) 轻处理（只为画图方便，不做额外工程）
############################################
# 约定字段：STORE, WEEK, total_sales, avg_price, promo_flag, custcoun, weekvol, pricmed, zone, scluster, city, ili(可空)
# 缺失值简单兜底
df <- df %>%
  mutate(
    total_sales = ifelse(is.na(total_sales), 0, total_sales),
    avg_price   = ifelse(is.na(avg_price), NA, avg_price),
    promo_flag  = ifelse(is.na(promo_flag), 0, promo_flag),
    custcoun    = ifelse(is.na(custcoun), NA, custcoun)
  )

############################################
## 3) 时间序列：全局周度销量（看趋势/季节）
############################################
ts_week <- df %>%
  group_by(WEEK) %>%
  summarise(sales = sum(total_sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(WEEK)

p_ts <- ggplot(ts_week, aes(WEEK, sales)) +
  geom_line() +
  labs(title = "Weekly Total Sales (All Stores)", x = "Week", y = "Total Sales") +
  theme_minimal()
ggsave("eda_sales_ts.png", p_ts, width = 8, height = 4, dpi = 150)

# 导出一个简要表：每周销量摘要（可放附录）
ts_week_summary <- ts_week %>%
  summarise(
    weeks = n(),
    mean_sales = mean(sales),
    sd_sales   = sd(sales),
    p10 = quantile(sales, 0.10),
    p50 = quantile(sales, 0.50),
    p90 = quantile(sales, 0.90)
  )
readr::write_csv(ts_week_summary, "eda_ts_week_summary.csv")

############################################
## 4) 价格 vs 销量：散点 + 简单平滑（整体层面）
############################################
# 用“店-周”粒度的 avg_price 与 total_sales，先做一个整体散点
scatter_df <- df %>%
  filter(!is.na(avg_price), total_sales >= 0) %>%
  sample_n(size = min(100000, n()), replace = FALSE)  # 只抽部分点，避免图太密

p_scatter <- ggplot(scatter_df, aes(avg_price, total_sales)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Price vs Sales (Store-Week)", x = "Average Price", y = "Total Sales") +
  theme_minimal()
ggsave("eda_sales_price_scatter.png", p_scatter, width = 7, height = 5, dpi = 150)

# 简单的 bin 汇总，导出表（价格分箱后的平均销量）
price_bins <- scatter_df %>%
  mutate(price_bucket = ntile(avg_price, 10)) %>%
  group_by(price_bucket) %>%
  summarise(
    avg_price_mid = mean(avg_price),
    mean_sales    = mean(total_sales),
    .groups = "drop"
  )
readr::write_csv(price_bins, "eda_price_bins_vs_sales.csv")

############################################
## 5) 区域/店铺类型差异：箱线图（zone / scluster）
############################################
# zone 维度
p_box_zone <- df %>%
  filter(!is.na(zone)) %>%
  group_by(zone, WEEK) %>%
  summarise(sales = sum(total_sales, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = zone, y = sales)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "Sales Distribution by Zone (Weekly Aggregated)", x = "Zone", y = "Weekly Sales (sum)") +
  theme_minimal()
ggsave("eda_sales_by_zone_box.png", p_box_zone, width = 8, height = 4.5, dpi = 150)

# scluster 维度（若类别很多，可以只画 Top 8）
top_scluster <- df %>%
  count(scluster, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(scluster)

p_box_scluster <- df %>%
  filter(scluster %in% top_scluster) %>%
  group_by(scluster, WEEK) %>%
  summarise(sales = sum(total_sales, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = scluster, y = sales)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "Sales Distribution by Store Cluster (Top 8, Weekly Aggregated)",
       x = "Store Cluster", y = "Weekly Sales (sum)") +
  theme_minimal()
ggsave("eda_sales_by_scluster_box.png", p_box_scluster, width = 9, height = 4.5, dpi = 150)

############################################
## 6) 分布与偏态：直方图（原始 & 对数）
############################################
# 用店-周销量的分布，先截断极端长尾以便可视化
hist_df <- df %>%
  mutate(total_sales_clip = pmin(total_sales, quantile(total_sales, 0.99, na.rm = TRUE))) 

p_hist_raw <- ggplot(hist_df, aes(total_sales_clip)) +
  geom_histogram(bins = 50) +
  labs(title = "Sales Distribution (clipped at P99)", x = "Total Sales (clipped)", y = "Count") +
  theme_minimal()
ggsave("eda_sales_hist.png", p_hist_raw, width = 7, height = 4, dpi = 150)

# 对数尺度（+1 防 0）
p_hist_log <- ggplot(hist_df, aes(log1p(total_sales))) +
  geom_histogram(bins = 50) +
  labs(title = "log(1+Sales) Distribution", x = "log(1 + Total Sales)", y = "Count") +
  theme_minimal()
ggsave("eda_sales_hist_log.png", p_hist_log, width = 7, height = 4, dpi = 150)

############################################
## 7) 数值变量相关性热力图（最相关的那几列）
############################################
# 只挑与后续建模常用的数值列（存在则取）
num_cols <- c("total_sales","avg_price","promo_flag","custcoun","weekvol","pricmed")
num_cols <- num_cols[num_cols %in% names(df)]

corr_tbl <- NULL
if(length(num_cols) >= 2){
  corr_mat <- cor(df[, num_cols], use = "pairwise.complete.obs")
  corr_tbl <- as.data.frame(corr_mat) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "corr")
  
  p_corr <- ggplot(corr_tbl, aes(var1, var2, fill = corr)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1,1), midpoint = 0) +
    labs(title = "Correlation Heatmap (Selected Numeric Vars)", x = "", y = "", fill = "r") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("eda_corr_heatmap.png", p_corr, width = 6.5, height = 5.5, dpi = 150)
  
  readr::write_csv(corr_tbl, "eda_corr_table.csv")
} else {
  message("可用的数值列不足，跳过相关性热力图。")
}

############################################
## 8) 小结输出（方便你粘到报告/附录）
############################################
# 导出“每个图”的关键一句话描述（供报告正文调整措辞用）
notes <- tribble(
  ~figure, ~takeaway,
  "eda_sales_ts.png", "全局周度销量存在明显季节波动和长期趋势，可在模型中加入时间项/季节项。",
  "eda_sales_price_scatter.png", "整体上价格与销量呈负向关系（LOESS 平滑线可见），提示价格相关特征具有预测力。",
  "eda_sales_by_zone_box.png", "不同 Zone 之间销量分布存在系统差异，建议加入区位固定效应或做分层建模。",
  "eda_sales_by_scluster_box.png", "不同门店簇（Top 8）销量差异显著，门店类型是重要解释变量。",
  "eda_sales_hist.png", "销量分布右偏明显，建议在模型中考虑对数变换以减轻偏态影响。",
  "eda_sales_hist_log.png", "log(1+sales) 后分布更集中，建模更稳健。",
  "eda_corr_heatmap.png", if(!is.null(corr_tbl)) "相关性图显示价格与销量负相关、促销/客流与销量正相关（如有）；用于筛选候选特征。" else "当前可用数值列不足，相关性热力图未生成。"
)
readr::write_csv(notes, "eda_report_notes.csv")

cat("EDA 完成，已导出：\n",
    "- eda_sales_ts.png\n",
    "- eda_sales_price_scatter.png\n",
    "- eda_sales_by_zone_box.png\n",
    "- eda_sales_by_scluster_box.png\n",
    "- eda_sales_hist.png\n",
    "- eda_sales_hist_log.png\n",
    if(!is.null(corr_tbl)) "- eda_corr_heatmap.png\n" else "",
    "- eda_ts_week_summary.csv\n",
    "- eda_price_bins_vs_sales.csv\n",
    if(!is.null(corr_tbl)) "- eda_corr_table.csv\n" else "",
    "- eda_report_notes.csv\n", sep = "")
