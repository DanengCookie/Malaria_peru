# 增强空间异质性的秘鲁疟疾温度归因月度空间分析
# Enhanced Peru Malaria Temperature Attribution with Spatial Heterogeneity
# 基于分组可视化和热点识别

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)      # pivot_longer，替代 reshape2::melt
library(readxl)
library(data.table)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(scales)
library(stringr)
library(classInt)  # 用于创建分组间隔
library(ggpubr)    # 用于提取图例

cat("========== 增强版秘鲁疟疾温度归因空间分析 ==========\n")

# 设置输出目录
output_dir <- "C:/Users/zengz/Desktop/秘鲁疟疾-雨潇第三版 - 自己实操"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 数据文件路径（根据实际路径调整）
attribution_data_path <- "C:/Users/zengz/Desktop/秘鲁疟疾-雨潇第三版 - 自己实操/泽哥代码/monthly_temperature_attribution_yuxiao.xlsx"
peru_map_path <- "C:/Users/zengz/Desktop/per_adm_ign_20200714_shp/per_admbnda_adm3_ign_20200714.shp"

##############################################################
# 1. 数据读取和预处理
##############################################################

cat("\n======= 数据读取和预处理 =======\n")

# 读取真实数据
attribution_data <- read_excel(attribution_data_path)
setDT(attribution_data)

# 数据清理
attribution_data[is.na(attribution_fraction_weighted), attribution_fraction_weighted := 0]
attribution_data[, month := as.integer(month)]
attribution_data[, year := as.integer(year)]
attribution_data[, ADM3_PCODE := as.character(ADM3_PCODE)]

cat("数据维度:", nrow(attribution_data), "行", ncol(attribution_data), "列\n")

# 读取地图数据
peru_map <- st_read(peru_map_path, quiet = TRUE)
if(st_crs(peru_map)$input != "EPSG:4326") {
  peru_map <- st_transform(peru_map, 4326)
}
peru_map$ADM3_PCODE <- as.character(peru_map$ADM3_PCODE)

##############################################################
# 2. 创建分组归因类别和热点识别
##############################################################

cat("\n======= 创建分组归因类别和热点识别 =======\n")

# 创建月度平均数据
monthly_avg_data <- attribution_data[, .(
  attribution_fraction_weighted = mean(attribution_fraction_weighted, na.rm = TRUE),
  total_malaria_cases = sum(total_malaria_cases, na.rm = TRUE),
  mean_temperature = mean(mean_temperature, na.rm = TRUE)
), by = .(ADM3_PCODE, month)]

# 创建归因分组的函数
create_attribution_categories <- function(data, method = "quantile") {
  
  if(method == "quantile") {
    # 使用五分位数创建分组
    breaks <- quantile(data$attribution_fraction_weighted, 
                       probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE)
    labels <- c("极低 (0-20%)", "低 (20-40%)", "中等 (40-60%)", 
                "高 (60-80%)", "极高 (80-100%)")
    
  } else if(method == "natural") {
    # 使用自然分组法
    breaks_obj <- classInt::classIntervals(data$attribution_fraction_weighted, 
                                           n = 5, style = "fisher")
    breaks <- breaks_obj$brks
    labels <- c("极低归因", "低归因", "中等归因", "高归因", "极高归因")
    
  } else if(method == "custom") {
    # 自定义阈值，突出热点
    breaks <- c(0, 0.05, 0.15, 0.30, 0.50, 1.0)
    labels <- c("<5%", "5–15%", "15–30%", 
                "30–50%", "Descriptive hotspot (>50%)")
  }
  
  data[, attribution_category := cut(attribution_fraction_weighted, 
                                     breaks = breaks, 
                                     labels = labels, 
                                     include.lowest = TRUE)]
  
  return(list(data = data, breaks = breaks, labels = labels))
}

# 为每个月创建分组数据
create_monthly_categorical_data <- function(selected_month, method = "custom") {
  
  month_data <- monthly_avg_data[month == selected_month]
  
  # 创建归因分组
  result <- create_attribution_categories(month_data, method)
  month_data <- result$data
  
  # 识别热点区域（直接使用"Descriptive hotspot (>50%)"类别）
  month_data[, is_hotspot := attribution_category == "Descriptive hotspot (>50%)"]
  month_data[is.na(is_hotspot), is_hotspot := FALSE]
  
  # 与地图合并
  peru_map_month <- peru_map %>%
    left_join(month_data, by = "ADM3_PCODE") %>%
    mutate(
      attribution_category = ifelse(is.na(attribution_category), 
                                    "No Data", as.character(attribution_category)),
      attribution_category = factor(attribution_category, 
                                    levels = c("No Data", result$labels)),
      is_hotspot = ifelse(is.na(is_hotspot), FALSE, is_hotspot),
      month = selected_month
    )
  
  return(list(map_data = peru_map_month, 
              breaks = result$breaks, 
              labels = result$labels))
}

##############################################################
# 3. 创建增强版科学可视化函数
##############################################################

cat("\n======= 创建增强版可视化函数 =======\n")

create_enhanced_peru_map <- function(month_num, categorical_data, 
                                     title_suffix = "", use_chinese = FALSE) {
  
  month_names_en <- c("January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December")
  month_names_cn <- c("一月", "二月", "三月", "四月", "五月", "六月",
                      "七月", "八月", "九月", "十月", "十一月", "十二月")
  
  month_name <- ifelse(use_chinese, month_names_cn[month_num], month_names_en[month_num])
  
  map_data <- categorical_data$map_data
  
  # 定义颜色方案 - 从冷色到暖色，突出高风险区域
  category_colors <- c(
    "No Data" = "#f0f0f0",
    "<5%" = "#d1e5f0",
    "5–15%" = "#92c5de", 
    "15–30%" = "#f7941d",
    "30–50%" = "#d73027",
    "Descriptive hotspot (>50%)" = "#8B0000"
  )
  
  # 计算统计信息
  stats <- map_data %>%
    st_drop_geometry() %>%
    filter(!is.na(attribution_fraction_weighted)) %>%
    summarise(
      mean_attr = mean(attribution_fraction_weighted, na.rm = TRUE),
      max_attr = max(attribution_fraction_weighted, na.rm = TRUE),
      hotspot_count = sum(attribution_category == "Descriptive hotspot (>50%)", na.rm = TRUE),
      mean_temp = mean(mean_temperature, na.rm = TRUE),
      total_cases = sum(total_malaria_cases, na.rm = TRUE)
    )
  
  # 主地图
  p <- ggplot(map_data) +
    geom_sf(aes(fill = attribution_category), 
            color = "white", size = 0.15, alpha = 0.9) +
    
    # 突出显示热点区域
    geom_sf(data = map_data[map_data$is_hotspot == TRUE, ], 
            aes(fill = attribution_category), 
            color = "#8B0000", size = 0.8, alpha = 0.95) +
    
    scale_fill_manual(
      values = category_colors,
      name = if(use_chinese) "归因风险等级" else "Model-estimated PAF (%)",
      na.value = "#f0f0f0",
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        ncol = 2,
        byrow = TRUE,
        override.aes = list(color = "white", size = 0.5)
      )
    ) +
    
    labs(
      title = if(use_chinese) 
        paste0("秘鲁疟疾温度归因热点分析 - ", month_name, title_suffix) else
          paste0("Peru Malaria Temperature Attribution Hotspots - ", month_name, title_suffix),
      
      subtitle = if(use_chinese)
        paste0("平均归因: ", sprintf("%.1f%%", stats$mean_attr * 100), 
               " | 最高: ", sprintf("%.1f%%", stats$max_attr * 100),
               " | 热点区域: ", stats$hotspot_count, "个",
               " | 总病例: ", format(stats$total_cases, big.mark = ",")) else
                 paste0("Spatial mean PAF: ", sprintf("%.1f%%", stats$mean_attr * 100),
                        " | Max: ", sprintf("%.1f%%", stats$max_attr * 100), 
                        " | Descriptive hotspots: ", stats$hotspot_count,
                        " | Total Cases: ", format(stats$total_cases, big.mark = ",")),
      
      caption = if(use_chinese) 
        paste0("红色边框标记热点区域 (>50%归因) | 基于2000-2023年数据") else
          paste0("Borders outline districts with mean PAF >50% | Based on 2000-2023 data")
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                color = "grey10", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, 
                                   color = "grey30", margin = margin(b = 10)),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "grey50"),
      
      legend.position = "bottom",
      legend.text = element_text(size = 10, color = "grey20"),
      legend.title = element_text(size = 12, face = "bold", color = "grey20"),
      legend.margin = margin(t = 15),
      
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  
  return(p)
}

##############################################################
# 4. 生成所有12个月的增强版地图
##############################################################

cat("\n======= 生成所有12个月的增强版地图 =======\n")

# 为每个月创建分类数据
monthly_categorical_data_list <- list()
for(month_num in 1:12) {
  monthly_categorical_data_list[[month_num]] <- create_monthly_categorical_data(month_num)
}

# 生成增强版地图 - 使用英文并添加"(2020-2023 Average)"
month_names_en <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")

enhanced_monthly_maps <- list()
for(month_num in 1:12) {
  cat("生成增强版地图 - 月份", month_num, "\n")
  
  # 调用原函数（title_suffix 置空，标题将在下面整体重构）
  base_map <- create_enhanced_peru_map(
    month_num,
    monthly_categorical_data_list[[month_num]],
    title_suffix = "",
    use_chinese = FALSE
  )
  
  # 提取当前月份的统计数据（用于 caption）
  stats_data <- monthly_categorical_data_list[[month_num]]$map_data %>%
    sf::st_drop_geometry() %>%
    filter(!is.na(attribution_fraction_weighted)) %>%
    summarise(
      mean_attr    = mean(attribution_fraction_weighted, na.rm = TRUE),
      max_attr     = max(attribution_fraction_weighted, na.rm = TRUE),
      hotspot_count = sum(attribution_category == "Descriptive hotspot (>50%)", na.rm = TRUE),
      total_cases  = sum(total_malaria_cases, na.rm = TRUE)
    )
  
  stats_caption <- paste0(
    "Spatial mean PAF: ", sprintf("%.1f%%", stats_data$mean_attr * 100),
    " | Max: ",          sprintf("%.1f%%", stats_data$max_attr * 100),
    " | Descriptive hotspots: ",     stats_data$hotspot_count,
    " | Total Cases: ",  format(stats_data$total_cases, big.mark = ","),
    "\nBorders outline districts with mean PAF >50% | Based on 2000-2023 data"
  )
  
  # 标题只保留月份名（size=45），去掉重复的固定描述行
  # 图例标题 size=30，图例文字 size=26，caption size=20
  enhanced_monthly_maps[[month_num]] <- base_map +
    labs(
      title    = month_names_en[month_num],   # 只显示月份，无第一行固定描述
      subtitle = NULL,                         # 不需要副标题
      caption  = stats_caption
    ) +
    theme(
      # 月份标题：size = 45
      plot.title    = element_text(size = 45, face = "bold", hjust = 0.5,
                                   color = "grey10", margin = margin(b = 10)),
      plot.subtitle = element_blank(),
      # caption：size = 20
      plot.caption  = element_text(size = 24, hjust = 0.5, color = "grey50",
                                   margin = margin(t = 8)),
      # 图例标题：size = 30；图例文字（区间标注）：size = 26
      legend.title  = element_text(size = 30, face = "bold", color = "grey20"),
      legend.text   = element_text(size = 26, color = "grey20"),
      legend.key.size = unit(0.9, "cm"),
      legend.margin   = margin(t = 18),
      # 图例位置沿用原函数 legend.position = "bottom"，此处不覆盖
      plot.margin   = margin(t = 15, r = 10, b = 10, l = 10)
    )
  
  # 图幅放大以容纳大字体标题与图例（width加宽给图例横排留空间）
  ggsave(
    filename = file.path(output_dir, sprintf("enhanced_month_%02d.png", month_num)),
    plot     = enhanced_monthly_maps[[month_num]],
    width    = 18, height = 15, dpi = 300, bg = "white"
  )
}

##############################################################
# 5. 创建月度热点统计分析
##############################################################

cat("\n======= 创建月度热点统计分析 =======\n")

# 统计每个月的热点信息
hotspot_stats <- data.frame()
for(month_num in 1:12) {
  month_data <- monthly_categorical_data_list[[month_num]]$map_data %>%
    st_drop_geometry()
  
  stats <- data.frame(
    month = month_num,
    month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[month_num],
    hotspot_count = sum(month_data$attribution_category == "Descriptive hotspot (>50%)", na.rm = TRUE),
    high_risk_count = sum(month_data$attribution_category == "30–50%", na.rm = TRUE),
    moderate_risk_count = sum(month_data$attribution_category == "15–30%", na.rm = TRUE),
    extreme_risk_count = sum(month_data$attribution_category == "Descriptive hotspot (>50%)", na.rm = TRUE),
    mean_attribution = mean(month_data$attribution_fraction_weighted, na.rm = TRUE),
    max_attribution = max(month_data$attribution_fraction_weighted, na.rm = TRUE),
    total_cases = sum(month_data$total_malaria_cases, na.rm = TRUE)
  )
  
  hotspot_stats <- rbind(hotspot_stats, stats)
}

# 创建热点分析图表
create_hotspot_analysis_plots <- function(hotspot_data, output_dir) {
  
  # 图1: 热点区域数量趋势
  scale_factor <- max(hotspot_data$hotspot_count) / max(hotspot_data$mean_attribution)
  
  hotspot_trend_plot <- ggplot(hotspot_data, aes(x = factor(month))) +
    geom_col(aes(y = hotspot_count), fill = "#d73027", alpha = 0.8, width = 0.7) +
    geom_line(aes(y = mean_attribution * scale_factor, group = 1), 
              color = "#1f78b4", linewidth = 1.2) +
    geom_point(aes(y = mean_attribution * scale_factor), 
               color = "#1f78b4", size = 3) +
    
    scale_x_discrete(labels = hotspot_data$month_name) +
    scale_y_continuous(
      name = "热点区域数量 Hotspot Count",
      sec.axis = sec_axis(~ . / scale_factor, name = "平均归因比例 Spatial mean PAF", 
                          labels = percent_format(accuracy = 0.1))
    ) +
    
    labs(
      title = "秘鲁疟疾温度归因热点区域月度分析",
      subtitle = "Hotspot Regions Analysis by Month (>50% Attribution)",
      x = "月份 Month",
      caption = "红柱: 热点区域数量 (>50%归因) | 蓝线: 平均归因比例"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      axis.title.y.left = element_text(color = "#d73027"),
      axis.title.y.right = element_text(color = "#1f78b4"),
      axis.text.x = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # 图2: 风险等级分布
  # 重新整理数据用于堆叠柱状图
  risk_data <- hotspot_data %>%
    select(month, month_name, high_risk_count, extreme_risk_count) %>%
    tidyr::pivot_longer(
      cols = c(high_risk_count, extreme_risk_count),
      names_to  = "risk_level",
      values_to = "count"
    ) %>%
    mutate(
      risk_level = factor(risk_level, 
                          levels = c("high_risk_count", "extreme_risk_count"),
                          labels = c("30–50%", "Descriptive hotspot (>50%)"))
    )
  
  risk_distribution_plot <- ggplot(risk_data, aes(x = factor(month), y = count, fill = risk_level)) +
    geom_col(position = "stack", alpha = 0.9, width = 0.8) +
    scale_fill_manual(
      values = c("30–50%" = "#f7941d", "Descriptive hotspot (>50%)" = "#8B0000"),
      name = "风险等级 Risk Level"
    ) +
    scale_x_discrete(labels = hotspot_data$month_name) +
    
    labs(
      title = "月度风险区域分布",
      subtitle = "Monthly Distribution of Risk Areas", 
      x = "月份 Month",
      y = "区域数量 Number of Areas"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  # 保存图片
  ggsave(file.path(output_dir, "hotspot_trend_analysis.png"), 
         hotspot_trend_plot, width = 14, height = 8, dpi = 300, bg = "white")
  
  ggsave(file.path(output_dir, "risk_distribution_analysis.png"), 
         risk_distribution_plot, width = 14, height = 8, dpi = 300, bg = "white")
  
  # 组合图
  combined_analysis <- grid.arrange(
    hotspot_trend_plot, risk_distribution_plot, 
    ncol = 1,
    top = grid::textGrob("秘鲁疟疾温度归因热点分析报告", 
                         gp = grid::gpar(fontsize = 16, font = 2))
  )
  
  ggsave(file.path(output_dir, "combined_hotspot_analysis.png"), 
         combined_analysis, width = 16, height = 14, dpi = 300, bg = "white")
  
  return(list(trend = hotspot_trend_plot, distribution = risk_distribution_plot))
}

# 生成分析图
analysis_plots <- create_hotspot_analysis_plots(hotspot_stats, output_dir)


##############################################################
# 7. 输出分析报告
##############################################################

cat("\n======= 生成分析报告 =======\n")

# 保存热点统计数据
write.csv(hotspot_stats, file.path(output_dir, "hotspot_monthly_statistics.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

# 识别年度最高风险月份和区域
peak_month <- hotspot_stats[which.max(hotspot_stats$hotspot_count), ]
high_risk_months <- hotspot_stats[hotspot_stats$hotspot_count > quantile(hotspot_stats$hotspot_count, 0.75), ]

analysis_report <- paste0(
  "秘鲁疟疾温度归因增强版空间分析报告\n",
  "Enhanced Peru Malaria Temperature Attribution Analysis Report\n",
  "================================================================\n\n",
  
  "热点定义 HOTSPOT DEFINITION:\n",
  "✓ 热点区域: 温度归因比例 >50% 的区域\n",
  "✓ Hotspot Areas: Regions with >50% temperature attribution\n\n",
  
  "分析改进 ANALYSIS IMPROVEMENTS:\n",
  "✓ 分组可视化: 将连续变量转为5个风险等级\n",
  "✓ 热点识别: 基于>50%归因阈值识别热点\n", 
  "✓ 颜色优化: 冷→暖色突出高风险区域\n",
  "✓ 空间异质性: 清晰显示地区差异\n",
  "✓ 统计分析: 月度热点趋势分析\n\n",
  
  "关键发现 KEY FINDINGS:\n",
  "- 热点区域最多月份: ", peak_month$month_name, 
  " (", peak_month$hotspot_count, "个热点)\n",
  "- 该月平均归因比例: ", sprintf("%.2f%%", peak_month$mean_attribution * 100), "\n",
  "- 高风险月份数: ", nrow(high_risk_months), "个月\n",
  "- 年度热点区域总数范围: ", min(hotspot_stats$hotspot_count), "-", 
  max(hotspot_stats$hotspot_count), "个\n\n",
  
  "输出文件 OUTPUT FILES:\n",
  "地图系列 Maps Series:\n",
  "  - enhanced_month_01.png 至 enhanced_month_12.png (单月地图)\n",
  "  - enhanced_peru_malaria_layout.png (12月组合布局)\n\n",
  
  "分析图表 Analysis Charts:\n",
  "  - hotspot_trend_analysis.png (热点趋势分析)\n",
  "  - risk_distribution_analysis.png (风险分布分析)\n",
  "  - combined_hotspot_analysis.png (综合分析图)\n\n",
  
  "数据文件 Data Files:\n",
  "  - hotspot_monthly_statistics.csv (热点月度统计)\n\n",
  
  "技术改进 TECHNICAL IMPROVEMENTS:\n",
  "- 使用自定义阈值替代连续梯度\n",
  "- 红色边框突出显示热点区域 (>50%归因)\n", 
  "- 分组图例增强空间对比度\n",
  "- 统计图表支持热点识别\n",
  "- 高分辨率输出(300 DPI)\n\n",
  
  "使用建议 USAGE RECOMMENDATIONS:\n",
  "- 政策制定: 重点关注红色边框标记的热点区域 (>50%归因)\n",
  "- 资源配置: 参考高风险月份进行预防部署\n",
  "- 科学研究: 使用hotspot_monthly_statistics.csv进行量化分析\n",
  "- 公共卫生: 基于风险等级制定分级应对策略\n\n",
  
  "输出目录: ", output_dir, "\n"
)

writeLines(analysis_report, file.path(output_dir, "enhanced_analysis_report.txt"))

cat("\n========== 增强版分析完成 ==========\n")
cat("主要改进:\n")
cat("✓ 使用真实数据进行分析\n")
cat("✓ 热点定义: >50%温度归因的区域\n")
cat("✓ 分组可视化显著增强空间异质性\n")
cat("✓ 优化的颜色方案突出高风险区域\n")
cat("✓ 统计分析支持政策决策\n")
cat("✓ 专业的双语标注和图例\n\n")

cat("关键文件:\n")
cat("- 主要布局图:", file.path(output_dir, "enhanced_peru_malaria_layout.png"), "\n")
cat("- 热点分析图:", file.path(output_dir, "combined_hotspot_analysis.png"), "\n")
cat("- 统计数据:", file.path(output_dir, "hotspot_monthly_statistics.csv"), "\n")