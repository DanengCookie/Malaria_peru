# 简化版疟疾温度分析：三个核心模型
# 恶性疟疾 (Y0) 和间日疟疾 (Y1) 分别分析
# 模型1：自然环境变量
# 模型2：+ 社会经济变量  
# 模型3：+ 4周温度湿度滞后变量

# 加载包
library(fixest)
library(data.table)
library(readxl)
library(writexl)
library(broom)

# 读取数据
data <- read_excel("C:/Users/zengz/Desktop/秘鲁疟疾/merged_data.xlsx")
setDT(data)

# 基础数据处理
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

# 创建4周滞后变量（温度、湿度、降水）
for(i in 1:4) {
  data[, paste0("temperature_lag", i) := shift(temperature, i, type = "lag"), by = cityid]
  data[, paste0("relative_humidity_lag", i) := shift(relative_humidity, i, type = "lag"), by = cityid]
  data[, paste0("precipitation_lag", i) := shift(total_precipitation_sum, i, type = "lag"), by = cityid]
  
  # 处理缺失值
  data[is.na(get(paste0("temperature_lag", i))), 
       (paste0("temperature_lag", i)) := mean(temperature, na.rm = TRUE), by = cityid]
  data[is.na(get(paste0("relative_humidity_lag", i))), 
       (paste0("relative_humidity_lag", i)) := mean(relative_humidity, na.rm = TRUE), by = cityid]
  data[is.na(get(paste0("precipitation_lag", i))), 
       (paste0("precipitation_lag", i)) := mean(total_precipitation_sum, na.rm = TRUE), by = cityid]
}

##############################################################
# 三个核心模型函数
##############################################################
build_three_models <- function(malaria_type, data) {
  
  cat(paste0("\n======= 分析 ", 
             ifelse(malaria_type == "Y0", "恶性疟疾 (P. falciparum)", "间日疟疾 (P. vivax)"), 
             " =======\n"))
  
  models <- list()
  
  ###############################################
  # 模型1：自然环境变量
  ###############################################
  cat(paste0("\n--- 模型1：自然环境变量 (", malaria_type, ") ---\n"))
  
  env_vars <- c("temperature", "temperature_sq", "pressure", "wind_speed", 
                "relative_humidity", "total_precipitation_sum")
  
  formula1 <- paste(malaria_type, "~", paste(env_vars, collapse = " + "), "| cityid + yearweek")
  
  if (offset_available) {
    model1 <- fepois(as.formula(formula1), offset = ~log_population_offset, data = data)
  } else {
    model1 <- fepois(as.formula(formula1), data = data)
  }
  
  print(summary(model1))
  models[["模型1_自然环境"]] <- model1
  
  ###############################################
  # 模型2：+ 社会经济变量
  ###############################################
  cat(paste0("\n--- 模型2：自然环境 + 社会经济 (", malaria_type, ") ---\n"))
  
  socio_vars <- env_vars
  
  # 添加可用的社会经济变量
  if("population_density" %in% names(data)) {
    socio_vars <- c(socio_vars, "log_population_density")
  }
  if("light_density" %in% names(data)) {
    socio_vars <- c(socio_vars, "log_light_density")
  }
  
  # 添加土地利用变量
  land_use_vars <- c("urban", "water", "forest", "farm")
  available_land_use <- intersect(land_use_vars, names(data))
  socio_vars <- c(socio_vars, available_land_use)
  
  formula2 <- paste(malaria_type, "~", paste(socio_vars, collapse = " + "), "| cityid + yearweek")
  
  if (offset_available) {
    model2 <- fepois(as.formula(formula2), offset = ~log_population_offset, data = data)
  } else {
    model2 <- fepois(as.formula(formula2), data = data)
  }
  
  print(summary(model2))
  models[["模型2_社会经济"]] <- model2
  
  ###############################################
  # 模型3：+ 4周温度和湿度滞后变量
  ###############################################
  cat(paste0("\n--- 模型3：完整模型 + 4周温度湿度滞后 (", malaria_type, ") ---\n"))
  
  lag_vars <- socio_vars
  
  # 添加温度和湿度的4周滞后变量
  for(i in 1:4) {
    lag_vars <- c(lag_vars, paste0("temperature_lag", i))
    lag_vars <- c(lag_vars, paste0("relative_humidity_lag", i))
  }
  
  formula3 <- paste(malaria_type, "~", paste(lag_vars, collapse = " + "), "| cityid + yearweek")
  
  if (offset_available) {
    model3 <- fepois(as.formula(formula3), offset = ~log_population_offset, data = data)
  } else {
    model3 <- fepois(as.formula(formula3), data = data)
  }
  
  print(summary(model3))
  models[["模型3_温湿度滞后"]] <- model3
  
  return(models)
}

##############################################################
# 运行分析
##############################################################

# 恶性疟疾分析
cat("\n\n========================================")
cat("\n          恶性疟疾分析开始")  
cat("\n========================================\n")
falciparum_models <- build_three_models("Y0", data)

# 间日疟疾分析
cat("\n\n========================================")
cat("\n          间日疟疾分析开始")
cat("\n========================================\n")
vivax_models <- build_three_models("Y1", data)

##############################################################
# 模型比较表
##############################################################
cat("\n\n======= 恶性疟疾：三个模型比较 =======\n")
falciparum_table <- etable(falciparum_models, 
                           title = "恶性疟疾 (P. falciparum) 模型比较",
                           digits = 4, se.below = TRUE)
print(falciparum_table)

cat("\n\n======= 间日疟疾：三个模型比较 =======\n")
vivax_table <- etable(vivax_models, 
                      title = "间日疟疾 (P. vivax) 模型比较",
                      digits = 4, se.below = TRUE)
print(vivax_table)

##############################################################
# 温度效应提取和比较
##############################################################
extract_temperature_effects <- function(models, malaria_name) {
  temp_results <- list()
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    coefs <- coef(model)
    ses <- se(model)
    
    if("temperature" %in% names(coefs)) {
      temp_coef <- coefs["temperature"]
      temp_se <- ses["temperature"]
      irr <- exp(temp_coef)
      ci_lower <- exp(temp_coef - 1.96 * temp_se)
      ci_upper <- exp(temp_coef + 1.96 * temp_se)
      
      temp_results[[model_name]] <- data.frame(
        疟疾类型 = malaria_name,
        模型 = model_name,
        温度系数 = round(temp_coef, 4),
        标准误 = round(temp_se, 4),
        IRR = round(irr, 4),
        CI下限 = round(ci_lower, 4),
        CI上限 = round(ci_upper, 4),
        P值 = round(2 * (1 - pnorm(abs(temp_coef/temp_se))), 4)
      )
    }
  }
  
  return(do.call(rbind, temp_results))
}

# 提取温度效应
falciparum_temp <- extract_temperature_effects(falciparum_models, "恶性疟疾")
vivax_temp <- extract_temperature_effects(vivax_models, "间日疟疾")
all_temp_effects <- rbind(falciparum_temp, vivax_temp)

cat("\n======= 温度效应汇总 =======\n")
print(all_temp_effects)

##############################################################
# 基本统计对比
##############################################################
cat("\n======= 疟疾类型基本统计对比 =======\n")
comparison_stats <- data[, .(
  恶性疟疾_总病例 = sum(Y0, na.rm = TRUE),
  恶性疟疾_平均 = round(mean(Y0, na.rm = TRUE), 2),
  恶性疟疾_标准差 = round(sd(Y0, na.rm = TRUE), 2),
  间日疟疾_总病例 = sum(Y1, na.rm = TRUE),
  间日疟疾_平均 = round(mean(Y1, na.rm = TRUE), 2),
  间日疟疾_标准差 = round(sd(Y1, na.rm = TRUE), 2),
  总观测数 = .N,
  城市数 = uniqueN(cityid),
  时间跨度 = paste0(min(year), "-", max(year))
)]
print(comparison_stats)

##############################################################
# 导出结果
##############################################################
cat("\n======= 导出结果到Excel =======\n")

excel_file <- "C:/Users/zengz/Desktop/秘鲁疟疾/2主模型/20261020malaria_three_models_results.xlsx"

excel_data <- list()

# 模型比较表
excel_data[["恶性疟疾_模型比较"]] <- as.data.frame(falciparum_table)
excel_data[["间日疟疾_模型比较"]] <- as.data.frame(vivax_table)

# 温度效应汇总
excel_data[["温度效应汇总"]] <- all_temp_effects

# 基本统计
excel_data[["基本统计对比"]] <- comparison_stats

# 各模型详细结果
for(name in names(falciparum_models)) {
  excel_data[[paste0("恶性疟疾_", name)]] <- broom::tidy(falciparum_models[[name]])
}

for(name in names(vivax_models)) {
  excel_data[[paste0("间日疟疾_", name)]] <- broom::tidy(vivax_models[[name]])
}

write_xlsx(excel_data, excel_file)
cat("结果已保存到:", excel_file, "\n")

##############################################################
# 最终总结
##############################################################
cat("\n======= 分析总结 =======\n")
cat("✓ 完成恶性疟疾和间日疟疾的三个核心模型分析\n")
cat("✓ 模型1：自然环境变量（温度、气压、风速、湿度、降水）\n")
cat("✓ 模型2：添加社会经济变量（人口密度、灯光密度、土地利用）\n")
cat("✓ 模型3：添加4周温度湿度滞后效应\n")
cat("✓ 所有结果已导出到Excel文件\n\n")

cat("关键发现需要关注：\n")
cat("1. 两种疟疾对温度的敏感性差异\n")
cat("2. 社会经济因素的作用\n")
cat("3. 温度湿度滞后效应的模式\n")
cat("4. 模型拟合度的改善程度\n")

cat("\n分析完成！\n")