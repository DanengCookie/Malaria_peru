library(fixest)
library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(writexl)  # 新增:用于导出Excel

##############################################################
# Helper function: Add transparency
##############################################################
add.alpha <- function(col, alpha=1) {
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) rgb(x[1], x[2], x[3], alpha))
}

##############################################################
# 数据预处理函数 - 与原始代码保持一致
##############################################################
prepare_vivax_data <- function(data, reference_temp = 16.0) {
  
  cat("\n=== 间日疟疾数据预处理 ===\n")
  
  # 基础数据处理 - 与原始代码保持一致
  data[, year := as.integer(year)]
  data[, week := as.integer(week)]
  data[, cityid := as.factor(cityid)]
  data[, yearweek := paste0(year, "_", sprintf("%02d", week))]
  data[, yearweek := as.factor(yearweek)]
  
  # 温度变量
  data[, temperature_sq := temperature^2]
  
  # 人口暴露项
  if ("population" %in% names(data)) {
    data[population <= 0 | is.na(population), population := 1]
    data[, log_population_offset := log(population)]
    offset_available <- TRUE
  } else {
    offset_available <- FALSE
  }
  
  # 社会经济变量处理
  if("population_density" %in% names(data)) {
    data[, log_population_density := log(population_density + 1)]
  }
  if("light_density" %in% names(data)) {
    data[, log_light_density := log(light_density + 1)]
  }
  
  # 为可视化创建centered温度变量
  data[, temperature_centered := temperature - reference_temp]
  data[, temperature_centered_sq := temperature_centered^2]
  
  cat("数据预处理完成\n")
  cat("温度范围:", round(range(data$temperature, na.rm = TRUE), 1), "°C\n")
  cat("间日疟疾(Y1)病例总数:", sum(data$Y1, na.rm = TRUE), "\n")
  
  return(list(data = data, offset_available = offset_available))
}

##############################################################
# 极端温度数据过滤函数 - 仅移除1%和99%分位数之外的数据
##############################################################
filter_extreme_temperatures <- function(data) {
  
  cat("\n=== 过滤极端温度数据(1%和99%分位数之外) ===\n")
  
  # 计算1%和99%分位数
  temp_p1 <- quantile(data$temperature, 0.01, na.rm = TRUE)
  temp_p99 <- quantile(data$temperature, 0.99, na.rm = TRUE)
  
  cat("原始数据行数:", nrow(data), "\n")
  cat("温度范围:", round(range(data$temperature, na.rm = TRUE), 1), "°C\n")
  cat("1%分位数:", round(temp_p1, 1), "°C\n")
  cat("99%分位数:", round(temp_p99, 1), "°C\n")
  
  # 过滤数据：保留1%-99%范围内的数据
  data_filtered <- data[data$temperature >= temp_p1 & 
                          data$temperature <= temp_p99, ]
  
  # 移除NA值
  data_filtered <- data_filtered[!is.na(data_filtered$temperature), ]
  
  cat("过滤后数据行数:", nrow(data_filtered), "\n")
  cat("移除数据量:", nrow(data) - nrow(data_filtered), "行 (", 
      round((nrow(data) - nrow(data_filtered))/nrow(data)*100, 1), "%)\n")
  cat("过滤后温度范围:", round(range(data_filtered$temperature, na.rm = TRUE), 1), "°C\n")
  
  return(data_filtered)
}

##############################################################
# 温度分箱函数 - 12个箱子，参考温度设置为16°C
##############################################################
create_temperature_bins <- function(data, temp_var = "temperature", n_bins = 12, reference_temp = 16) {
  
  cat("\n=== 创建温度分箱 (12分箱模式) ===\n")
  cat("目标参考温度:", reference_temp, "°C\n")
  cat("分箱数量:", n_bins, "\n")
  
  # 计算温度分位数
  temp_quantiles <- quantile(data[[temp_var]], 
                             probs = seq(0, 1, length.out = n_bins + 1), 
                             na.rm = TRUE)
  
  cat("温度分位数切点:\n")
  print(round(temp_quantiles, 1))
  
  # 创建温度分箱
  data$temp_bin <- cut(data[[temp_var]], 
                       breaks = temp_quantiles,
                       include.lowest = TRUE,
                       labels = paste0("tbin", 1:n_bins))
  
  # 找到最接近16°C的分箱
  temp_bin_centers <- data %>%
    group_by(temp_bin) %>%
    summarise(temp_center = mean(.data[[temp_var]], na.rm = TRUE), .groups = 'drop')
  
  closest_bin_idx <- which.min(abs(temp_bin_centers$temp_center - reference_temp))
  reference_bin <- paste0("tbin", closest_bin_idx)
  
  cat("选定参考分箱:", reference_bin, ", 中心温度:", 
      round(temp_bin_centers$temp_center[closest_bin_idx], 1), "°C\n")
  
  data$temp_bin <- relevel(factor(data$temp_bin), ref = reference_bin)
  
  # 创建分箱统计表
  bin_stats <- data %>%
    group_by(temp_bin) %>%
    summarise(
      n_obs = n(),
      temp_min = round(min(.data[[temp_var]], na.rm = TRUE), 1),
      temp_max = round(max(.data[[temp_var]], na.rm = TRUE), 1),
      temp_mean = round(mean(.data[[temp_var]], na.rm = TRUE), 1),
      cases_total = sum(Y1, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(temp_range = paste0(temp_min, "-", temp_max, "°C"))
  
  cat("\n12个温度分箱统计:\n")
  print(bin_stats[, c("temp_bin", "temp_range", "temp_mean", "n_obs", "cases_total")])
  
  return(list(data = data, bin_stats = bin_stats, reference_bin = reference_bin))
}

##############################################################
# 估计间日疟疾分箱模型 - 支持12个分箱
##############################################################
estimate_vivax_binned_model <- function(data, reference_bin, n_bins = 12, offset_available = TRUE) {
  
  cat("\n=== 估计间日疟疾温度分箱模型 (12分箱) ===\n")
  cat("参考分箱:", reference_bin, "\n")
  
  ref_num <- as.numeric(gsub("tbin", "", reference_bin))
  
  # 构建模型项（排除参考分箱）
  temp_terms <- c()
  for(i in 1:n_bins) {
    if(i != ref_num) {
      temp_terms <- c(temp_terms, paste0("temp_tbin", i))
    }
  }
  
  # 创建分箱指示变量
  for(i in 1:n_bins) {
    bin_name <- paste0("tbin", i)
    data[[paste0("temp_", bin_name)]] <- as.numeric(data$temp_bin == bin_name)
  }
  
  # 环境控制变量
  control_vars <- c("pressure", "wind_speed", "relative_humidity", "total_precipitation_sum")
  
  # 社会经济变量
  if("log_population_density" %in% names(data)) {
    control_vars <- c(control_vars, "log_population_density")
  }
  if("log_light_density" %in% names(data)) {
    control_vars <- c(control_vars, "log_light_density")
  }
  
  # 土地利用变量
  land_use_vars <- c("urban", "water", "forest", "farm")
  available_land_use <- intersect(land_use_vars, names(data))
  control_vars <- c(control_vars, available_land_use)
  
  formula_str <- paste("Y1 ~", 
                       paste(control_vars, collapse = " + "), "+",
                       paste(temp_terms, collapse = " + "),
                       "| cityid + yearweek")
  
  cat("模型估计中 (包含", length(temp_terms), "个温度分箱项)...\n")
  
  # 估计模型
  if (offset_available) {
    model <- fepois(as.formula(formula_str), 
                    offset = ~log_population_offset, 
                    data = data)
  } else {
    model <- fepois(as.formula(formula_str), data = data)
  }
  
  cat("12分箱模型估计完成，系数数量:", length(coef(model)), "\n")
  
  return(model)
}

##############################################################
# 计算分箱预测 - 支持12个分箱
##############################################################
calculate_vivax_binned_predictions <- function(model, bin_stats, reference_bin, n_bins = 12, alpha = 0.05) {
  
  cat("\n=== 计算12分箱预测 ===\n")
  
  coef_est <- coef(model)
  vcov_matrix <- vcov(model, cluster = ~cityid)
  
  results <- data.frame()
  ref_num <- as.numeric(gsub("tbin", "", reference_bin))
  
  for(i in 1:n_bins) {
    bin_name <- paste0("tbin", i)
    bin_info <- bin_stats[bin_stats$temp_bin == bin_name, ]
    
    if(i == ref_num) {
      # 参考分箱系数为0
      coef_val <- 0
      se_val <- 0
    } else {
      # 非参考分箱
      var_name <- paste0("temp_tbin", i)
      
      if(var_name %in% names(coef_est)) {
        coef_val <- coef_est[var_name]
        se_val <- sqrt(vcov_matrix[var_name, var_name])
      } else {
        coef_val <- 0
        se_val <- 0
      }
    }
    
    # 计算相对风险比和置信区间
    rr <- exp(coef_val)
    ci_lower <- exp(coef_val - qnorm(1 - alpha/2) * se_val)
    ci_upper <- exp(coef_val + qnorm(1 - alpha/2) * se_val)
    
    # P值计算
    z_score <- coef_val / se_val
    p_value <- 2 * (1 - pnorm(abs(z_score)))
    
    # 显著性标记
    sig_mark <- ifelse(p_value < 0.001, "***",
                       ifelse(p_value < 0.01, "**",
                              ifelse(p_value < 0.05, "*", "")))
    
    results <- rbind(results, data.frame(
      bin_number = i,
      temp_bin = bin_name,
      temp_range = bin_info$temp_range,
      temp_mean = bin_info$temp_mean,
      temp_min = bin_info$temp_min,
      temp_max = bin_info$temp_max,
      n_obs = bin_info$n_obs,
      cases_total = bin_info$cases_total,
      coefficient = coef_val,
      std_error = se_val,
      relative_risk = rr,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      significance = sig_mark,
      is_reference = (i == ref_num)
    ))
  }
  
  cat("12分箱预测计算完成\n")
  cat("风险比范围:", round(range(results$relative_risk), 2), "\n")
  
  return(results)
}

##############################################################
# 【新增功能】导出温度分箱结果表格到Excel
##############################################################
export_binned_results_table <- function(pred_results, save_path, filename = "vivax_temperature_binned_RR_table.xlsx") {
  
  cat("\n=== 导出温度分箱结果表格 ===\n")
  
  # 创建格式化的结果表
  output_table <- pred_results %>%
    arrange(bin_number) %>%
    mutate(
      温度分箱 = paste0("T", bin_number),
      温度范围_摄氏度 = temp_range,
      平均温度_摄氏度 = round(temp_mean, 2),
      观测数 = n_obs,
      病例数 = cases_total,
      回归系数 = round(coefficient, 4),
      标准误 = round(std_error, 4),
      相对风险RR = round(relative_risk, 3),
      RR下限_95CI = round(ci_lower, 3),
      RR上限_95CI = round(ci_upper, 3),
      置信区间95 = paste0("(", round(ci_lower, 3), ", ", round(ci_upper, 3), ")"),
      P值 = ifelse(p_value < 0.001, "<0.001", sprintf("%.4f", p_value)),
      显著性 = significance,
      是否参考组 = ifelse(is_reference, "是", "否")
    ) %>%
    select(温度分箱, 温度范围_摄氏度, 平均温度_摄氏度, 观测数, 病例数,
           回归系数, 标准误, 相对风险RR, RR下限_95CI, RR上限_95CI, 
           置信区间95, P值, 显著性, 是否参考组)
  
  # 保存到Excel
  full_path <- file.path(save_path, filename)
  write_xlsx(list(
    "温度分箱RR值表" = output_table,
    "原始数据" = pred_results
  ), full_path)
  
  cat("✓ 表格已保存至:", full_path, "\n")
  cat("✓ 包含两个工作表:\n")
  cat("  1. 温度分箱RR值表 - 格式化的中文表格\n")
  cat("  2. 原始数据 - 英文完整数据\n\n")
  
  # 打印预览
  cat("=== 表格预览（前5行）===\n")
  print(head(output_table, 5))
  
  return(list(
    table = output_table,
    file_path = full_path
  ))
}

##############################################################
# 【新增功能】生成简洁的RR值汇总表
##############################################################
create_rr_summary_table <- function(pred_results) {
  
  cat("\n=== 生成RR值汇总表 ===\n")
  
  summary_table <- pred_results %>%
    arrange(bin_number) %>%
    mutate(
      温度分箱 = paste0("T", bin_number, ifelse(is_reference, " (参考)", "")),
      温度_摄氏度 = paste0(temp_mean, "°C"),
      RR及95CI = ifelse(is_reference, 
                       "1.00 (参考)",
                       paste0(sprintf("%.2f", relative_risk), 
                              " (", sprintf("%.2f", ci_lower), 
                              "-", sprintf("%.2f", ci_upper), ")",
                              significance))
    ) %>%
    select(温度分箱, 温度_摄氏度, RR及95CI)
  
  print(summary_table)
  
  return(summary_table)
}

##############################################################
# 数据检查函数
##############################################################
check_vivax_binned_data_requirements <- function(data) {
  
  cat("\n=== 数据要求检查 ===\n")
  
  required_vars <- c("Y1", "temperature", "cityid", "year", "week",
                     "pressure", "wind_speed", "relative_humidity", 
                     "total_precipitation_sum")
  
  missing_vars <- setdiff(required_vars, names(data))
  
  if(length(missing_vars) > 0) {
    cat("❌ 缺少必需变量:", paste(missing_vars, collapse = ", "), "\n")
    return(FALSE)
  } else {
    cat("✓ 所有必需变量都存在\n")
  }
  
  # 检查最小样本量
  min_samples_needed <- 12 * 50  # 每个分箱至少50个样本
  actual_samples <- nrow(data)
  
  if(actual_samples < min_samples_needed) {
    cat("警告: 样本量可能不足，建议至少", min_samples_needed, "行数据进行12分箱分析\n")
    cat("当前样本量:", actual_samples, "行\n")
  } else {
    cat("✓ 样本量充足，支持12分箱分析\n")
    cat("预期每个分箱样本:", round(actual_samples/12, 0), "行\n")
  }
  
  cat("数据检查完成\n\n")
  return(TRUE)
}

##############################################################
# 森林图绘制函数 - 12分箱
##############################################################
create_vivax_forest_plot_12bins <- function(results, reference_bin, save_path) {
  
  cat("\n=== 创建12分箱森林图 ===\n")
  
  ref_num <- as.numeric(gsub("tbin", "", reference_bin))
  
  results <- results %>%
    arrange(bin_number) %>%
    mutate(
      bin_label = paste0("T", bin_number, ": ", temp_range),
      is_ref = (bin_number == ref_num)
    )
  
  max_ci <- max(results$ci_upper, na.rm = TRUE)
  x_limit <- min(max_ci * 1.1, 5)
  
  p <- ggplot(results, aes(x = relative_risk, y = reorder(bin_label, -bin_number))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                   height = 0.3, size = 0.8, color = "#2C3E50") +
    geom_point(aes(fill = is_ref, size = is_ref, shape = is_ref), 
               color = "#2C3E50", stroke = 1) +
    scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
                      labels = c("TRUE" = "参考组", "FALSE" = "比较组"),
                      name = "") +
    scale_size_manual(values = c("TRUE" = 5, "FALSE" = 4), guide = "none") +
    scale_shape_manual(values = c("TRUE" = 23, "FALSE" = 21), guide = "none") +
    scale_x_continuous(
      limits = c(0, x_limit),
      breaks = seq(0, ceiling(x_limit), by = 0.5),
      expand = c(0.02, 0)
    ) +
    labs(
      title = "间日疟疾温度分箱效应分析 (12分箱)",
      subtitle = paste0("参考温度组: T", ref_num, " (~16°C)"),
      x = "相对风险比 (Relative Risk, RR) 及 95% 置信区间",
      y = "温度分箱 (°C)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.text.x = element_text(size = 11),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.8, "cm"),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # 保存高分辨率图像
  filename <- file.path(save_path, "vivax_temperature_forest_plot_12bins.png")
  ggsave(filename, plot = p, width = 12, height = 10, dpi = 300, bg = "white")
  
  cat("✓ 12分箱森林图已保存:", filename, "\n")
  
  return(list(plot = p, file = filename))
}

##############################################################
# 分层可视化图表 - 12分箱
##############################################################
plot_vivax_binned_with_distribution_12bins <- function(results, temp_data, data, 
                                                       reference_bin, save_path) {
  
  cat("\n=== 创建12分箱分层可视化图表 ===\n")
  
  ref_num <- as.numeric(gsub("tbin", "", reference_bin))
  
  results <- results %>%
    arrange(bin_number) %>%
    mutate(color_group = case_when(
      bin_number == ref_num ~ "参考组",
      relative_risk > 1 ~ "高风险",
      TRUE ~ "低风险"
    ))
  
  # 面板A: RR曲线图
  p1 <- ggplot(results, aes(x = temp_mean, y = relative_risk)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", size = 0.8) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = add.alpha("#3498DB", 0.2), color = NA) +
    geom_line(color = "#2C3E50", size = 1.2) +
    geom_point(aes(fill = color_group), size = 4, shape = 21, color = "#2C3E50", stroke = 1) +
    scale_fill_manual(
      values = c("参考组" = "#E74C3C", "高风险" = "#E67E22", "低风险" = "#3498DB"),
      name = "风险分类"
    ) +
    labs(
      title = "A. 温度与间日疟疾相对风险关系",
      x = NULL,
      y = "相对风险比 (RR)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  # 面板B: 温度分布直方图
  p2 <- ggplot(data, aes(x = temperature)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, 
                   fill = "#3498DB", alpha = 0.6, color = "white") +
    geom_density(color = "#2C3E50", size = 1) +
    geom_vline(data = results[results$bin_number == ref_num, ],
               aes(xintercept = temp_mean), 
               linetype = "dashed", color = "#E74C3C", size = 1) +
    labs(
      title = "B. 温度分布",
      x = "温度 (°C)",
      y = "密度"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # 组合图表
  library(gridExtra)
  combined_plot <- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
  
  # 保存
  filename <- file.path(save_path, "vivax_temperature_binned_distribution_12bins.png")
  ggsave(filename, plot = combined_plot, width = 12, height = 10, dpi = 300, bg = "white")
  
  cat("✓ 12分箱分层图已保存:", filename, "\n")
  
  return(filename)
}

##############################################################
# 主分析函数 - 间日疟疾温度分箱分析【已完善】
##############################################################
run_vivax_binned_analysis <- function(data_path = "c:/Users/zengz/Desktop/秘鲁疟疾-雨潇第三版 - 自己实操/泽哥代码/merged_data.xlsx", 
                                      n_bins = 12,
                                      reference_temp = 16.0,
                                      save_path = "c:/Users/zengz/Desktop/秘鲁疟疾-雨潇第三版 - 自己实操/泽哥代码/分箱模型") {
  
  cat("=== 开始间日疟疾温度分箱分析 ===\n")
  cat("原始数据路径:", data_path, "\n")
  cat("分箱数量:", n_bins, "\n")
  cat("参考温度:", reference_temp, "°C\n")
  cat("结果保存路径:", save_path, "\n")
  
  # 确保保存路径存在
  if(!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cat("创建保存路径:", save_path, "\n")
  }
  
  # 读取数据
  data <- read_excel(data_path)
  setDT(data)
  
  cat("原始数据行数:", nrow(data), "\n")
  
  # 数据检查
  if(!check_vivax_binned_data_requirements(data)) {
    stop("数据检查失败，请检查数据格式")
  }
  
  # 步骤1：数据预处理
  processed_data <- prepare_vivax_data(data, reference_temp)
  data_processed <- processed_data$data
  offset_available <- processed_data$offset_available
  
  # 步骤2：过滤极端数据
  data_filtered <- filter_extreme_temperatures(data_processed)
  
  # 步骤3：创建温度分箱
  binning_result <- create_temperature_bins(data_filtered, n_bins = n_bins, reference_temp = reference_temp)
  data_binned <- binning_result$data
  bin_stats <- binning_result$bin_stats
  reference_bin <- binning_result$reference_bin
  
  # 步骤4：估计分箱模型
  model_binned <- estimate_vivax_binned_model(data_binned, reference_bin, n_bins, offset_available)
  
  # 步骤5：计算分箱预测
  pred_results <- calculate_vivax_binned_predictions(model_binned, bin_stats, reference_bin, n_bins)
  
  # 【新增】步骤6：导出RR值表格到Excel
  table_result <- export_binned_results_table(pred_results, save_path)
  
  # 【新增】步骤7：打印RR值汇总表
  summary_table <- create_rr_summary_table(pred_results)
  
  # 步骤8：创建森林图
  forest_result <- create_vivax_forest_plot_12bins(pred_results, reference_bin, save_path)
  
  # 步骤9：创建分层可视化图表
  layered_plot_file <- plot_vivax_binned_with_distribution_12bins(
    results = pred_results,
    temp_data = data_filtered$temperature,
    data = data_filtered,
    reference_bin = reference_bin,
    save_path = save_path
  )
  
  # 结果验证
  ref_results <- pred_results[pred_results$bin_number == as.numeric(gsub("tbin", "", reference_bin)), ]
  cat("\n=== 参考点验证 ===\n")
  cat("参考点风险比应该接近1.0:\n")
  print(ref_results[, c("temp_range", "temp_mean", "relative_risk")])
  
  # 极值分析
  cat("\n=== 极值分析 ===\n")
  lowest_temp <- pred_results[pred_results$bin_number == 1, ]
  highest_temp <- pred_results[pred_results$bin_number == max(pred_results$bin_number), ]
  
  cat("最低温度段 (T1):\n")
  cat("  温度范围:", lowest_temp$temp_range, "\n")
  cat("  风险比:", round(lowest_temp$relative_risk, 2), 
      " (95% CI:", round(lowest_temp$ci_lower, 2), "-", round(lowest_temp$ci_upper, 2), ")\n")
  
  cat("最高温度段 (T", n_bins, "):\n", sep="")
  cat("  温度范围:", highest_temp$temp_range, "\n")
  cat("  风险比:", round(highest_temp$relative_risk, 2),
      " (95% CI:", round(highest_temp$ci_lower, 2), "-", round(highest_temp$ci_upper, 2), ")\n")
  
  # 找到风险最高的分箱
  max_risk_bin <- pred_results[which.max(pred_results$relative_risk), ]
  
  cat("\n风险最高分箱:\n")
  cat("  T", max_risk_bin$bin_number, " (", max_risk_bin$temp_range, "), RR=", 
      round(max_risk_bin$relative_risk, 2), 
      " (95% CI:", round(max_risk_bin$ci_lower, 2), "-", round(max_risk_bin$ci_upper, 2), ")\n")
  
  # 模型质量评估
  cat("\n=== 模型质量评估 ===\n")
  model_aic <- AIC(model_binned)
  cat("模型AIC:", format(model_aic, scientific = FALSE), "\n")
  
  # 显著性统计
  coef_summary <- summary(model_binned)
  temp_coefs <- coef_summary$coefficients[grepl("temp_tbin", rownames(coef_summary$coefficients)), ]
  significant_coefs <- sum(temp_coefs[, "Pr(>|z|)"] < 0.05, na.rm = TRUE)
  
  cat("显著温度分箱项:", significant_coefs, "项 (", 
      round(significant_coefs/nrow(temp_coefs)*100, 1), "%)\n")
  
  cat("\n=== 间日疟疾温度分箱分析完成 ===\n")
  cat("生成的文件:\n")
  cat("1. RR值表格:", table_result$file_path, "\n")
  cat("2. 森林图:", forest_result$file, "\n")
  cat("3. 分层图:", layered_plot_file, "\n")
  
  return(list(
    model = model_binned,
    predictions = pred_results,
    rr_table = table_result$table,
    rr_table_file = table_result$file_path,
    summary_table = summary_table,
    forest_plot = forest_result$plot,
    forest_file = forest_result$file,
    layered_plot_file = layered_plot_file,
    reference_bin = reference_bin,
    bin_stats = bin_stats,
    data_original = data,
    data_filtered = data_filtered,
    save_path = save_path,
    n_bins = n_bins,
    model_aic = model_aic,
    significant_coefs = significant_coefs
  ))
}

##############################################################
# 结果解释函数
##############################################################
interpret_vivax_binned_results <- function(results) {
  
  cat("\n=== 间日疟疾结果解释 ===\n")
  
  pred_results <- results$predictions
  
  # 找到风险最高和最低的分箱
  cat("间日疟疾风险分析:\n")
  max_risk_bin <- pred_results[which.max(pred_results$relative_risk), ]
  min_risk_bin <- pred_results[which.min(pred_results$relative_risk), ]
  
  cat("  最高风险分箱: T", max_risk_bin$bin_number, 
      " (", max_risk_bin$temp_range, ", ", max_risk_bin$temp_mean, "°C), RR=", 
      round(max_risk_bin$relative_risk, 2), "\n")
  cat("  最低风险分箱: T", min_risk_bin$bin_number, 
      " (", min_risk_bin$temp_range, ", ", min_risk_bin$temp_mean, "°C), RR=", 
      round(min_risk_bin$relative_risk, 2), "\n")
  
  # 统计显著性分析
  cat("\n统计显著性分析:\n")
  cat("  模型AIC:", format(results$model_aic, scientific = FALSE), "\n")
  cat("  显著分箱项:", results$significant_coefs, "项\n")
  cat("  分箱数量:", results$n_bins, "个分箱\n")
  cat("  样本量:", nrow(results$data_filtered), "行\n")
  
  # 临界温度阈值识别
  cat("\n临界温度阈值识别:\n")
  
  # 高风险阈值 (RR > 1.5)
  high_risk_bins <- pred_results[pred_results$relative_risk > 1.5, ]
  if(nrow(high_risk_bins) > 0) {
    min_high_temp <- min(high_risk_bins$temp_mean)
    cat("  高风险温度阈值: ≥", round(min_high_temp, 1), "°C\n")
  }
  
  # 低风险阈值 (RR < 0.8)
  low_risk_bins <- pred_results[pred_results$relative_risk < 0.8, ]
  if(nrow(low_risk_bins) > 0) {
    max_low_temp <- max(low_risk_bins$temp_mean)
    cat("  低风险温度阈值: ≤", round(max_low_temp, 1), "°C\n")
  }
  
  cat("\n=== 间日疟疾结果解释完成 ===\n")
}

##############################################################
# 使用示例和执行
##############################################################

cat("=== 间日疟疾温度分箱模型分析代码（已完善）===\n")
cat("✓ 新增功能: 导出温度分箱RR值表格到Excel\n")
cat("✓ 新增功能: 打印RR值汇总表\n\n")

cat("主要功能:\n")
cat("1. ✅ 温度分箱模型 - 12个分箱，参考温度16°C\n")
cat("2. ✅ 【新】导出RR值表格 - Excel格式，包含所有统计信息\n")
cat("3. ✅ 【新】打印RR汇总表 - 控制台快速查看\n")
cat("4. ✅ 森林图可视化 - 清晰显示各温度区间效应\n")
cat("5. ✅ 分层可视化图表 - 风险曲线 + 温度分布\n\n")

cat("使用方法:\n")
cat("results <- run_vivax_binned_analysis(\n")
cat("  data_path = 'D:/已结项目数据/秘鲁大论文全部/秘鲁大论文全部/merged_data.xlsx',\n")
cat("  n_bins = 12,\n")
cat("  reference_temp = 16.0,\n")
cat("  save_path = 'C:/Users/a1230/Desktop/jqxx'\n")
cat(")\n\n")

cat("输出文件:\n")
cat("1. vivax_temperature_binned_RR_table.xlsx - 【新】RR值表格\n")
cat("2. vivax_temperature_forest_plot_12bins.png - 森林图\n")
cat("3. vivax_temperature_binned_distribution_12bins.png - 分层图\n\n")

cat("=== 运行分析 ===\n")
# 自动运行分析
results_vivax_binned <- run_vivax_binned_analysis()

cat("\n=== 间日疟疾温度分箱分析完成 ===\n")
cat("可用结果:\n")
cat("- results_vivax_binned$rr_table: RR值表格（数据框）\n")
cat("- results_vivax_binned$rr_table_file: RR值表格文件路径\n")
cat("- results_vivax_binned$summary_table: RR汇总表\n")
cat("- results_vivax_binned$predictions: 完整预测结果\n")
cat("- results_vivax_binned$model: 模型对象\n")
cat("- results_vivax_binned$forest_file: 森林图文件路径\n")
cat("- results_vivax_binned$layered_plot_file: 分层图文件路径\n\n")

# 解释结果
interpret_vivax_binned_results(results_vivax_binned)

cat("\n=== 分析完成 ===\n")