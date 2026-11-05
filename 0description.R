# =============================================================================
# 秘鲁论文数据描述性统计分析
# 分析变量: temperature, pressure, wind_speed, relative_humidity, 
#          total_precipitation_sum, population_density, light_density, 
#          population, urban, water, forest, farm
# =============================================================================

# 加载必要的包
library(readxl)      # 读取Excel文件
library(dplyr)       # 数据处理
library(tidyr)       # 数据整理 (包含gather函数)
library(psych)       # 描述性统计
library(knitr)       # 表格输出
library(ggplot2)     # 可视化
library(corrplot)    # 相关性图

# 设置工作目录和文件路径
file_path <- "C:/Users/zengz/Desktop/秘鲁疟疾/merged_data.xlsx"

# 读取数据
cat("正在读取数据文件...\n")
data <- read_excel(file_path)

# 查看数据基本信息
cat("数据维度:", dim(data), "\n")
cat("变量名称:\n")
print(names(data))

# 定义要分析的连续型变量
continuous_vars <- c("temperature", "pressure", "wind_speed", "relative_humidity", 
                     "total_precipitation_sum", "population_density", "light_density", 
                     "population", "urban", "water", "forest", "farm")

# 检查变量是否存在于数据中
existing_vars <- continuous_vars[continuous_vars %in% names(data)]
missing_vars <- continuous_vars[!continuous_vars %in% names(data)]

if(length(missing_vars) > 0) {
  cat("警告: 以下变量在数据中不存在:\n")
  print(missing_vars)
}

cat("将分析以下变量:\n")
print(existing_vars)

# 选择要分析的变量
analysis_data <- data[, existing_vars, drop = FALSE]

# =============================================================================
# 1. 基本描述性统计
# =============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("基本描述性统计结果\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# 计算均值和标准差
descriptive_stats <- analysis_data %>%
  summarise_all(list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    n = ~sum(!is.na(.x)),
    na_count = ~sum(is.na(.x))
  ))

# 转置数据框以便更好地显示
descriptive_stats_t <- data.frame(
  Variable = existing_vars,
  N = as.numeric(descriptive_stats[grep("_n$", names(descriptive_stats))]),
  Missing = as.numeric(descriptive_stats[grep("_na_count$", names(descriptive_stats))]),
  Mean = round(as.numeric(descriptive_stats[grep("_mean$", names(descriptive_stats))]), 4),
  SD = round(as.numeric(descriptive_stats[grep("_sd$", names(descriptive_stats))]), 4),
  Min = round(as.numeric(descriptive_stats[grep("_min$", names(descriptive_stats))]), 4),
  Max = round(as.numeric(descriptive_stats[grep("_max$", names(descriptive_stats))]), 4)
)

# 显示结果表格
print(kable(descriptive_stats_t, 
            caption = "连续型变量描述性统计表",
            align = c("l", rep("r", 6))))

# =============================================================================
# 2. 使用psych包进行更详细的描述性统计
# =============================================================================

cat("\n详细描述性统计 (使用psych包):\n")
detailed_stats <- describe(analysis_data)
print(round(detailed_stats, 4))

# =============================================================================
# 3. 按变量单独展示均值和标准差
# =============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("各变量均值和标准差汇总\n")
cat(paste(rep("=", 50), collapse=""), "\n")

for(var in existing_vars) {
  if(var %in% names(analysis_data)) {
    var_data <- analysis_data[[var]]
    var_mean <- mean(var_data, na.rm = TRUE)
    var_sd <- sd(var_data, na.rm = TRUE)
    var_n <- sum(!is.na(var_data))
    
    cat(sprintf("%-25s: 均值 = %8.4f, 标准差 = %8.4f (N = %d)\n", 
                var, var_mean, var_sd, var_n))
  }
}

# =============================================================================
# 4. 数据质量检查
# =============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("数据质量检查\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# 缺失值统计
missing_summary <- analysis_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(Missing_Percentage = round(Missing_Count / nrow(analysis_data) * 100, 2)) %>%
  arrange(desc(Missing_Count))

print(kable(missing_summary, 
            caption = "缺失值统计",
            col.names = c("变量", "缺失数量", "缺失百分比(%)")))

# =============================================================================
# 5. 保存结果到指定文件夹
# =============================================================================

# 设置输出文件夹路径
output_dir <- "C:/Users/zengz/Desktop/秘鲁疟疾/输出"

# 检查文件夹是否存在，如果不存在则创建
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("已创建文件夹:", output_dir, "\n")
}

# 创建结果列表
results_list <- list(
  basic_stats = descriptive_stats_t,
  detailed_stats = detailed_stats,
  missing_summary = missing_summary
)

# 定义输出文件路径
rdata_path <- file.path(output_dir, "peru_descriptive_results.RData")
basic_csv_path <- file.path(output_dir, "peru_basic_descriptive_stats.csv")
detailed_csv_path <- file.path(output_dir, "peru_detailed_stats.csv")
missing_csv_path <- file.path(output_dir, "peru_missing_summary.csv")

# 保存为RData文件
save(results_list, file = rdata_path)

# 保存为CSV文件
write.csv(descriptive_stats_t, basic_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(detailed_stats, detailed_csv_path, fileEncoding = "UTF-8")
write.csv(missing_summary, missing_csv_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n结果已保存到以下文件:\n")
cat("- ", rdata_path, " (R数据文件)\n")
cat("- ", basic_csv_path, " (基本统计CSV)\n")
cat("- ", detailed_csv_path, " (详细统计CSV)\n")
cat("- ", missing_csv_path, " (缺失值统计CSV)\n")

# =============================================================================
# 额外：创建论文格式的描述性统计表格
# =============================================================================

# 创建专门用于论文的格式化表格
paper_table <- descriptive_stats_t %>%
  select(Variable, N, Mean, SD) %>%
  mutate(
    Mean_SD = paste0(sprintf("%.4f", Mean), " (", sprintf("%.4f", SD), ")"),
    Variable_Chinese = case_when(
      Variable == "temperature" ~ "温度",
      Variable == "pressure" ~ "压力", 
      Variable == "wind_speed" ~ "风速",
      Variable == "relative_humidity" ~ "相对湿度",
      Variable == "total_precipitation_sum" ~ "总降水量",
      Variable == "population_density" ~ "人口密度",
      Variable == "light_density" ~ "光密度",
      Variable == "population" ~ "人口",
      Variable == "urban" ~ "城市",
      Variable == "water" ~ "水域",
      Variable == "forest" ~ "森林", 
      Variable == "farm" ~ "农田",
      TRUE ~ Variable
    )
  ) %>%
  select(Variable, Variable_Chinese, N, Mean, SD, Mean_SD)

# 保存论文格式表格
paper_table_path <- file.path(output_dir, "peru_paper_format_table.csv")
write.csv(paper_table, paper_table_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("- ", paper_table_path, " (论文格式表格)\n")

# 输出论文格式表格预览
cat("\n论文格式描述性统计表格预览:\n")
cat(paste(rep("=", 60), collapse=""), "\n")
print(kable(paper_table[, c("Variable_Chinese", "N", "Mean", "SD", "Mean_SD")],
            col.names = c("变量", "样本量", "均值", "标准差", "均值(标准差)"),
            caption = "连续型变量描述性统计表"))


# =============================================================================
# 6. 可选：创建简单的可视化
# =============================================================================

cat("\n创建描述性统计可视化图表...\n")

# 创建均值条形图
mean_plot_data <- descriptive_stats_t %>%
  select(Variable, Mean) %>%
  mutate(Variable = reorder(Variable, Mean))

p1 <- ggplot(mean_plot_data, aes(x = Variable, y = Mean)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "各变量均值比较",
       x = "变量",
       y = "均值") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 创建标准差条形图  
sd_plot_data <- descriptive_stats_t %>%
  select(Variable, SD) %>%
  mutate(Variable = reorder(Variable, SD))

p2 <- ggplot(sd_plot_data, aes(x = Variable, y = SD)) +
  geom_col(fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(title = "各变量标准差比较",
       x = "变量", 
       y = "标准差") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 定义图表保存路径
means_plot_path <- file.path(output_dir, "peru_means_plot.png")
sd_plot_path <- file.path(output_dir, "peru_sd_plot.png")

# 保存图表到指定文件夹
ggsave(means_plot_path, p1, width = 10, height = 6, dpi = 300)
ggsave(sd_plot_path, p2, width = 10, height = 6, dpi = 300)

cat("可视化图表已保存:\n")
cat("- ", means_plot_path, " (均值比较图)\n")
cat("- ", sd_plot_path, " (标准差比较图)\n")

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("分析完成！\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("\n在文件夹 C:/Users/a1230/Desktop/jqxx 中，你将找到以下文件:\n")
cat("\n📊 描述性统计表格:\n")
cat("  • peru_basic_descriptive_stats.csv - 基本描述统计(均值、标准差等)\n")
cat("  • peru_detailed_stats.csv - 详细统计(包含偏度、峰度等)\n") 
cat("  • peru_paper_format_table.csv - 论文格式表格(中英文对照)\n")
cat("  • peru_missing_summary.csv - 缺失值统计\n")
cat("\n📈 可视化图表:\n") 
cat("  • peru_means_plot.png - 各变量均值对比图\n")
cat("  • peru_sd_plot.png - 各变量标准差对比图\n")
cat("\n💾 数据文件:\n")
cat("  • peru_descriptive_results.RData - 完整分析结果(R格式)\n")
cat("\n建议重点使用 peru_paper_format_table.csv，它包含了你论文需要的\n")
cat("所有描述性统计信息，格式规整，可直接用于学术写作。\n")