# =============================================================================
# Malaria Incidence Rate and Case Count Geographic Visualization Maps (Modified with Improved Legends)
# Author: Data Analyst
# Date: September 2025
# Function: Create geographic maps showing P. falciparum, P. vivax, and total malaria incidence rates and case counts
# Modified: Improved legend bar length and 4-point segmentation for better readability
# =============================================================================

# Clear environment
rm(list = ls())

# Load necessary packages
library(readxl)       # Read Excel files
library(data.table)   # Efficient data processing
library(dplyr)        # Data manipulation
library(tidyr)        # Data reshaping
library(ggplot2)      # Data visualization
library(sf)           # Spatial data processing
library(viridis)      # Color schemes
library(scales)       # Format labels
library(gridExtra)    # Combine plots
library(grid)         # For textGrob
library(RColorBrewer) # Additional color schemes
library(writexl)      # Write Excel files

# Create output directory
output_dir <- "C:/Users/zengz/Desktop/秘鲁疟疾/3空间发病率可视化/malaria_spatial_analysis"
dir.create(output_dir, showWarnings = FALSE)

# =============================================================================
# 1. Data Reading and Preprocessing
# =============================================================================

cat("\n======= Reading and Preprocessing Malaria Data =======\n")

# Read data
data <- read_excel("C:/Users/zengz/Desktop/秘鲁疟疾/merged_data.xlsx")

# Convert to data.table for improved processing efficiency
setDT(data)

# Check data structure
cat("Data dimensions:", dim(data), "\n")
cat("Column names:", colnames(data)[1:10], "...\n")

# Determine ID column for map matching
if("ADM3" %in% colnames(data)) {
  id_col <- "ADM3"
  cat("Found ADM3 column for map matching\n")
} else if("ADM3_PCODE" %in% colnames(data)) {
  id_col <- "ADM3_PCODE"
  cat("Found ADM3_PCODE column for map matching\n")
} else {
  id_col <- "cityid"
  cat("Using cityid for map matching\n")
}

# Data preprocessing
data[, year := as.integer(year)]
data[, week := as.integer(week)]
data[, cityid := as.factor(cityid)]
data[, Y0 := as.numeric(Y0)]  # P. falciparum
data[, Y1 := as.numeric(Y1)]  # P. vivax
data[, Y := as.numeric(Y)]    # Total malaria

# Handle missing values
data[is.na(Y0), Y0 := 0]
data[is.na(Y1), Y1 := 0]
data[is.na(Y), Y := 0]

# Check population data availability
if("population" %in% names(data)) {
  data[, population := as.numeric(population)]
  data[population <= 0 | is.na(population), population := 1]  # Avoid division by 0
  cat("Population data available for incidence rate calculation\n")
  population_available <- TRUE
} else {
  cat("Warning: No population data available, will only create case count maps\n")
  population_available <- FALSE
}

cat("P. falciparum (Y0) statistics:\n")
print(summary(data$Y0))
cat("P. vivax (Y1) statistics:\n")
print(summary(data$Y1))
cat("Total malaria (Y) statistics:\n")
print(summary(data$Y))
if(population_available) {
  cat("Population statistics:\n")
  print(summary(data$population))
}

# =============================================================================
# 2. Calculate Both Case Counts and Incidence Rates by Administrative Unit
# =============================================================================

cat("\n======= Calculating Case Counts and Incidence Rates by Administrative Unit =======\n")

# Aggregate data by administrative unit (city/ADM3)
city_analysis_data <- data %>%
  group_by(cityid) %>%
  summarise(
    # Retain ID for map matching
    ADM3_ID = first(get(id_col)),
    
    # Total cases over study period (ABSOLUTE COUNTS)
    total_falciparum_cases = sum(Y0, na.rm = TRUE),
    total_vivax_cases = sum(Y1, na.rm = TRUE),
    total_malaria_cases = sum(Y, na.rm = TRUE),
    
    # Population and study period information
    avg_population = if(population_available) mean(population, na.rm = TRUE) else NA,
    total_weeks = n(),
    years_covered = n_distinct(year),
    year_range = paste(min(year), "-", max(year)),
    
    # Calculate incidence rates per 1,000 people per year (only if population available)
    falciparum_incidence_per_1000 = if(population_available) (total_falciparum_cases / avg_population / years_covered) * 1000 else NA,
    vivax_incidence_per_1000 = if(population_available) (total_vivax_cases / avg_population / years_covered) * 1000 else NA,
    total_incidence_per_1000 = if(population_available) (total_malaria_cases / avg_population / years_covered) * 1000 else NA,
    
    # Calculate incidence rates per 100,000 people per year (only if population available)
    falciparum_incidence_per_100k = if(population_available) (total_falciparum_cases / avg_population / years_covered) * 100000 else NA,
    vivax_incidence_per_100k = if(population_available) (total_vivax_cases / avg_population / years_covered) * 100000 else NA,
    total_incidence_per_100k = if(population_available) (total_malaria_cases / avg_population / years_covered) * 100000 else NA,
    
    # Calculate case density (cases per week)
    falciparum_cases_per_week = total_falciparum_cases / total_weeks,
    vivax_cases_per_week = total_vivax_cases / total_weeks,
    total_cases_per_week = total_malaria_cases / total_weeks,
    
    # Calculate case proportions
    falciparum_proportion = ifelse(total_malaria_cases > 0, total_falciparum_cases / total_malaria_cases * 100, 0),
    vivax_proportion = ifelse(total_malaria_cases > 0, total_vivax_cases / total_malaria_cases * 100, 0),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(total_malaria_cases))  # Sort by total case count

# Handle infinite or NaN values
if(population_available) {
  city_analysis_data <- city_analysis_data %>%
    mutate(
      falciparum_incidence_per_1000 = ifelse(is.infinite(falciparum_incidence_per_1000) | is.nan(falciparum_incidence_per_1000), 0, falciparum_incidence_per_1000),
      vivax_incidence_per_1000 = ifelse(is.infinite(vivax_incidence_per_1000) | is.nan(vivax_incidence_per_1000), 0, vivax_incidence_per_1000),
      total_incidence_per_1000 = ifelse(is.infinite(total_incidence_per_1000) | is.nan(total_incidence_per_1000), 0, total_incidence_per_1000),
      falciparum_incidence_per_100k = ifelse(is.infinite(falciparum_incidence_per_100k) | is.nan(falciparum_incidence_per_100k), 0, falciparum_incidence_per_100k),
      vivax_incidence_per_100k = ifelse(is.infinite(vivax_incidence_per_100k) | is.nan(vivax_incidence_per_100k), 0, vivax_incidence_per_100k),
      total_incidence_per_100k = ifelse(is.infinite(total_incidence_per_100k) | is.nan(total_incidence_per_100k), 0, total_incidence_per_100k)
    )
}

# Handle proportion calculations
city_analysis_data <- city_analysis_data %>%
  mutate(
    falciparum_proportion = ifelse(is.infinite(falciparum_proportion) | is.nan(falciparum_proportion), 0, falciparum_proportion),
    vivax_proportion = ifelse(is.infinite(vivax_proportion) | is.nan(vivax_proportion), 0, vivax_proportion)
  )

cat("City-level analysis data summary:\n")
print(head(city_analysis_data, 10))

# Save analysis data
write_xlsx(city_analysis_data, file.path(output_dir, "city_malaria_analysis_data.xlsx"))
write.csv(city_analysis_data, file.path(output_dir, "city_malaria_analysis_data.csv"), row.names = FALSE)

# =============================================================================
# 3. Load Map Data
# =============================================================================

cat("\n======= Loading Map Data =======\n")

# Read shapefile map data
map_path <- "C:/Users/zengz/Desktop/per_adm_ign_20200714_shp/per_admbnda_adm3_ign_20200714.shp"
tryCatch({
  map_data <- st_read(map_path, quiet = TRUE)
  cat("Successfully read map data containing", nrow(map_data), "administrative regions\n")
  
  # Check shapefile structure
  map_cols <- colnames(map_data)
  cat("Map data columns:", paste(head(map_cols, 10), collapse=", "), 
      ifelse(length(map_cols) > 10, "...", ""), "\n")
  
  # Determine matching field
  if("ADM3_PCODE" %in% map_cols) {
    map_id_field <- "ADM3_PCODE"
    cat("Using ADM3_PCODE field for matching\n")
  } else if("ADM3" %in% map_cols) {
    map_id_field <- "ADM3"
    cat("Using ADM3 field for matching\n")
  } else {
    # Find potential ID fields
    potential_fields <- grep("ADM|ID|CODE|PCODE", map_cols, value = TRUE)
    if(length(potential_fields) > 0) {
      map_id_field <- potential_fields[1]
      cat("Using", map_id_field, "field for matching\n")
    } else {
      map_id_field <- map_cols[1]
      cat("Warning: Using", map_id_field, "field for matching (may not be correct)\n")
    }
  }
  
  # Prepare data for map matching
  analysis_for_map <- city_analysis_data %>%
    rename(!!map_id_field := ADM3_ID)
  
  # Merge map data and analysis data
  map_with_data <- map_data %>%
    left_join(analysis_for_map, by = map_id_field)
  
  # Check matching results
  matched_regions <- sum(!is.na(map_with_data$total_malaria_cases))
  total_regions <- nrow(map_with_data)
  match_rate <- matched_regions / total_regions * 100
  
  cat("Map matching results: Successfully matched", matched_regions, "/", total_regions, 
      "regions (", round(match_rate, 1), "%)\n")
  
  if(match_rate < 30) {
    cat("Warning: Low match rate, maps may not show complete data\n")
  }
  
}, error = function(e) {
  cat("Error reading map data:", e$message, "\n")
  cat("Will skip map creation\n")
  map_data <- NULL
  map_with_data <- NULL
})

# =============================================================================
# 4. Create Case Count Maps with Improved Legends
# =============================================================================

cat("\n======= Creating Case Count Maps with Improved Legends =======\n")

# Only proceed if map data was loaded successfully
if(!is.null(map_with_data)) {
  
  # Define custom color palettes
  falciparum_colors <- c("#FEF0F0", "#FDCCCB", "#FC9F9C", "#E74C3C", "#C0392B", "#922B21")  # 红色系
  vivax_colors <- c("#EBF5FB", "#A9CCE3", "#7FB3D3", "#3498DB", "#2980B9", "#1F4E79")      # 蓝色系  
  total_colors <- c("#EAFAF1", "#A9DFBF", "#7DCEA0", "#27AE60", "#239B56", "#196F3D")      # 绿色系
  
  # Map 1: P. falciparum total case count (Improved legend)
  falciparum_count_map <- ggplot(map_with_data) +
    geom_sf(aes(fill = total_falciparum_cases), color = "white", size = 0.1) +
    scale_fill_gradientn(
      colors = falciparum_colors,
      name = "Total Cases",
      na.value = "grey90",
      trans = "sqrt",
      breaks = scales::pretty_breaks(n = 4),  # 4个分段点
      labels = scales::comma,
      guide = guide_colorbar(
        barwidth = 25,        # 拉长图例条
        barheight = 0.8,      # 增加高度
        title.position = "top",
        label.hjust = 0.5,    # 标签居中
        title.hjust = 0.5,    # 标题居中
        nbin = 100            # 增加颜色渐变平滑度
      )
    ) +
    labs(
      title = "P. falciparum Total Case Count by Administrative District",
      subtitle = paste0("Cumulative cases over study period", 
                        ifelse(population_available, " (with population data)", " (no population data)"))
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.2),
      panel.grid.minor = element_line(color = "grey95", size = 0.1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = 10, b = 10)  # 增加图例边距
    )
  
  # Map 2: P. vivax total case count (Improved legend)
  vivax_count_map <- ggplot(map_with_data) +
    geom_sf(aes(fill = total_vivax_cases), color = "white", size = 0.1) +
    scale_fill_gradientn(
      colors = vivax_colors,
      name = "Total Cases",
      na.value = "grey90",
      trans = "sqrt",
      breaks = scales::pretty_breaks(n = 4),  # 4个分段点
      labels = scales::comma,
      guide = guide_colorbar(
        barwidth = 25,        # 拉长图例条
        barheight = 0.8,      # 增加高度
        title.position = "top",
        label.hjust = 0.5,    # 标签居中
        title.hjust = 0.5,    # 标题居中
        nbin = 100            # 增加颜色渐变平滑度
      )
    ) +
    labs(
      title = "P. vivax Total Case Count by Administrative District",
      subtitle = paste0("Cumulative cases over study period", 
                        ifelse(population_available, " (with population data)", " (no population data)"))
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.2),
      panel.grid.minor = element_line(color = "grey95", size = 0.1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = 10, b = 10)  # 增加图例边距
    )
  
  # Map 3: Total malaria case count (Improved legend)
  total_count_map <- ggplot(map_with_data) +
    geom_sf(aes(fill = total_malaria_cases), color = "white", size = 0.1) +
    scale_fill_gradientn(
      colors = total_colors,
      name = "Total Cases",
      na.value = "grey90",
      trans = "sqrt",
      breaks = scales::pretty_breaks(n = 4),  # 4个分段点
      labels = scales::comma,
      guide = guide_colorbar(
        barwidth = 25,        # 拉长图例条
        barheight = 0.8,      # 增加高度
        title.position = "top",
        label.hjust = 0.5,    # 标签居中
        title.hjust = 0.5,    # 标题居中
        nbin = 100            # 增加颜色渐变平滑度
      )
    ) +
    labs(
      title = "Total Malaria Case Count by Administrative District",
      subtitle = paste0("Cumulative cases over study period", 
                        ifelse(population_available, " (with population data)", " (no population data)"))
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.2),
      panel.grid.minor = element_line(color = "grey95", size = 0.1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = 10, b = 10)  # 增加图例边距
    )
  
  # Save individual case count maps
  ggsave(file.path(output_dir, "falciparum_case_count_map.png"), 
         falciparum_count_map, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "vivax_case_count_map.png"), 
         vivax_count_map, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "total_case_count_map.png"), 
         total_count_map, width = 12, height = 8, dpi = 300)
  
  cat("Case count maps with improved legends saved successfully\n")
  
  # =============================================================================
  # 5. Create Incidence Rate Maps with Improved Legends (if population data available)
  # =============================================================================
  
  if(population_available) {
    cat("\n======= Creating Incidence Rate Maps with Improved Legends =======\n")
    
    # Map 1: P. falciparum incidence rate per 1,000 people (Improved legend)
    falciparum_incidence_map <- ggplot(map_with_data) +
      geom_sf(aes(fill = falciparum_incidence_per_1000), color = "white", size = 0.1) +
      scale_fill_gradientn(
        colors = falciparum_colors,
        name = "Incidence Rate\n(per 1,000/year)",
        na.value = "grey90",
        trans = "sqrt",
        breaks = scales::pretty_breaks(n = 4),  # 4个分段点
        labels = function(x) format(round(x, 1), nsmall = 1),
        guide = guide_colorbar(
          barwidth = 25,        # 拉长图例条
          barheight = 0.8,      # 增加高度
          title.position = "top",
          label.hjust = 0.5,    # 标签居中
          title.hjust = 0.5,    # 标题居中
          nbin = 100            # 增加颜色渐变平滑度
        )
      ) +
      labs(
        title = "P. falciparum Incidence Rate by Administrative District",
        subtitle = "Annual incidence per 1,000 people (population offset considered)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_line(color = "grey95", size = 0.1),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 10, b = 10)  # 增加图例边距
      )
    
    # Map 2: P. vivax incidence rate per 1,000 people (Improved legend)
    vivax_incidence_map <- ggplot(map_with_data) +
      geom_sf(aes(fill = vivax_incidence_per_1000), color = "white", size = 0.1) +
      scale_fill_gradientn(
        colors = vivax_colors,
        name = "Incidence Rate\n(per 1,000/year)",
        na.value = "grey90",
        trans = "sqrt",
        breaks = scales::pretty_breaks(n = 4),  # 4个分段点
        labels = function(x) format(round(x, 1), nsmall = 1),
        guide = guide_colorbar(
          barwidth = 25,        # 拉长图例条
          barheight = 0.8,      # 增加高度
          title.position = "top",
          label.hjust = 0.5,    # 标签居中
          title.hjust = 0.5,    # 标题居中
          nbin = 100            # 增加颜色渐变平滑度
        )
      ) +
      labs(
        title = "P. vivax Incidence Rate by Administrative District",
        subtitle = "Annual incidence per 1,000 people (population offset considered)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_line(color = "grey95", size = 0.1),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 10, b = 10)  # 增加图例边距
      )
    
    # Map 3: Total malaria incidence rate per 1,000 people (Improved legend)
    total_incidence_map <- ggplot(map_with_data) +
      geom_sf(aes(fill = total_incidence_per_1000), color = "white", size = 0.1) +
      scale_fill_gradientn(
        colors = total_colors,
        name = "Incidence Rate\n(per 1,000/year)",
        na.value = "grey90",
        trans = "sqrt",
        breaks = scales::pretty_breaks(n = 4),  # 4个分段点
        labels = function(x) format(round(x, 1), nsmall = 1),
        guide = guide_colorbar(
          barwidth = 25,        # 拉长图例条
          barheight = 0.8,      # 增加高度
          title.position = "top",
          label.hjust = 0.5,    # 标签居中
          title.hjust = 0.5,    # 标题居中
          nbin = 100            # 增加颜色渐变平滑度
        )
      ) +
      labs(
        title = "Total Malaria Incidence Rate by Administrative District",
        subtitle = "Annual incidence per 1,000 people (population offset considered)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_line(color = "grey95", size = 0.1),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 10, b = 10)  # 增加图例边距
      )
    
    # Save individual incidence rate maps
    ggsave(file.path(output_dir, "falciparum_incidence_rate_map.png"), 
           falciparum_incidence_map, width = 12, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "vivax_incidence_rate_map.png"), 
           vivax_incidence_map, width = 12, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "total_incidence_rate_map.png"), 
           total_incidence_map, width = 12, height = 8, dpi = 300)
    
    cat("Incidence rate maps with improved legends saved successfully\n")
  } else {
    cat("Skipping incidence rate maps (no population data)\n")
  }
  
  # =============================================================================
  # 6. Create Combined Maps with Consistent Style
  # =============================================================================
  
  cat("\n======= Creating Combined Maps with Consistent Style =======\n")
  
  # Combined case count maps
  combined_case_count_maps <- grid.arrange(
    falciparum_count_map, vivax_count_map, total_count_map,
    ncol = 1, nrow = 3,
    top = textGrob("Malaria Case Counts by Species in Peru", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  # Save combined case count maps
  ggsave(file.path(output_dir, "combined_malaria_case_count_maps.png"), 
         combined_case_count_maps, width = 12, height = 16, dpi = 300)
  
  # Side-by-side comparison for case counts
  case_count_comparison <- grid.arrange(
    falciparum_count_map, vivax_count_map,
    ncol = 2, nrow = 1,
    top = textGrob("Malaria Case Count Comparison by Species", 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  ggsave(file.path(output_dir, "malaria_case_count_comparison.png"), 
         case_count_comparison, width = 20, height = 8, dpi = 300)
  
  if(population_available) {
    # Combined incidence rate maps
    combined_incidence_maps <- grid.arrange(
      falciparum_incidence_map, vivax_incidence_map, total_incidence_map,
      ncol = 1, nrow = 3,
      top = textGrob("Malaria Incidence Rates by Species in Peru", 
                     gp = gpar(fontsize = 16, fontface = "bold"))
    )
    
    ggsave(file.path(output_dir, "combined_malaria_incidence_rate_maps.png"), 
           combined_incidence_maps, width = 12, height = 16, dpi = 300)
    
    # Create comprehensive comparison: case counts vs incidence rates
    comprehensive_comparison <- grid.arrange(
      total_count_map, total_incidence_map,
      ncol = 2, nrow = 1,
      top = textGrob("Total Malaria: Case Counts vs Incidence Rates", 
                     gp = gpar(fontsize = 16, fontface = "bold"))
    )
    
    ggsave(file.path(output_dir, "case_count_vs_incidence_rate_comparison.png"), 
           comprehensive_comparison, width = 20, height = 8, dpi = 300)
  }
  
  cat("Combined maps with improved legends saved successfully\n")
  
} else {
  cat("Skipping map creation due to map data loading error\n")
}

# =============================================================================
# 7. Create Summary Statistics and Charts
# =============================================================================

cat("\n======= Creating Summary Statistics and Charts =======\n")

# Calculate summary statistics
analysis_summary <- city_analysis_data %>%
  summarise(
    total_cities = n(),
    
    # Case count statistics
    total_falciparum_cases = sum(total_falciparum_cases, na.rm = TRUE),
    total_vivax_cases = sum(total_vivax_cases, na.rm = TRUE),
    total_malaria_cases = sum(total_malaria_cases, na.rm = TRUE),
    
    falciparum_mean_cases = mean(total_falciparum_cases, na.rm = TRUE),
    falciparum_median_cases = median(total_falciparum_cases, na.rm = TRUE),
    falciparum_max_cases = max(total_falciparum_cases, na.rm = TRUE),
    
    vivax_mean_cases = mean(total_vivax_cases, na.rm = TRUE),
    vivax_median_cases = median(total_vivax_cases, na.rm = TRUE),
    vivax_max_cases = max(total_vivax_cases, na.rm = TRUE),
    
    total_mean_cases = mean(total_malaria_cases, na.rm = TRUE),
    total_median_cases = median(total_malaria_cases, na.rm = TRUE),
    total_max_cases = max(total_malaria_cases, na.rm = TRUE),
    
    # Incidence rate statistics (if available)
    falciparum_mean_incidence = if(population_available) mean(falciparum_incidence_per_1000, na.rm = TRUE) else NA,
    vivax_mean_incidence = if(population_available) mean(vivax_incidence_per_1000, na.rm = TRUE) else NA,
    total_mean_incidence = if(population_available) mean(total_incidence_per_1000, na.rm = TRUE) else NA,
    
    # Population information
    total_population = if(population_available) sum(avg_population, na.rm = TRUE) else NA
  )

print("Analysis Summary Statistics:")
print(analysis_summary)

# Create bar chart of top 20 cities by total case count
top_cities_cases_plot <- city_analysis_data %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(cityid, total_malaria_cases), y = total_malaria_cases)) +
  geom_bar(stat = "identity", fill = "#27AE60", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 20 Cities by Total Malaria Case Count",
    subtitle = paste0("Cumulative cases over study period", 
                      ifelse(population_available, " (with population data)", " (no population data)")),
    x = "City ID",
    y = "Total Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

# Create comparison chart for species (case counts)
species_cases_comparison_data <- city_analysis_data %>%
  select(cityid, total_falciparum_cases, total_vivax_cases) %>%
  slice_head(n = 15) %>%
  pivot_longer(cols = c(total_falciparum_cases, total_vivax_cases),
               names_to = "species", values_to = "case_count") %>%
  mutate(species = case_when(
    species == "total_falciparum_cases" ~ "P. falciparum",
    species == "total_vivax_cases" ~ "P. vivax"
  ))

species_cases_comparison_plot <- ggplot(species_cases_comparison_data, 
                                        aes(x = reorder(cityid, case_count), 
                                            y = case_count, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Malaria Case Count Comparison by Species (Top 15 Cities)",
    subtitle = paste0("Cumulative cases over study period", 
                      ifelse(population_available, " (with population data)", " (no population data)")),
    x = "City ID",
    y = "Total Cases",
    fill = "Malaria Species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11, face = "italic")
  )

# Create incidence rate comparison chart (if available)
if(population_available) {
  # Sort by incidence rate for this chart
  city_analysis_data_by_incidence <- city_analysis_data %>%
    arrange(desc(total_incidence_per_1000))
  
  top_cities_incidence_plot <- city_analysis_data_by_incidence %>%
    slice_head(n = 20) %>%
    ggplot(aes(x = reorder(cityid, total_incidence_per_1000), y = total_incidence_per_1000)) +
    geom_bar(stat = "identity", fill = "#2980B9", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Top 20 Cities by Total Malaria Incidence Rate",
      subtitle = "Annual incidence per 1,000 people (population offset considered)",
      x = "City ID",
      y = "Incidence Rate (per 1,000/year)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  ggsave(file.path(output_dir, "top_cities_incidence_rate.png"), 
         top_cities_incidence_plot, width = 12, height = 8, dpi = 300)
}

# Save charts
ggsave(file.path(output_dir, "top_cities_case_count.png"), 
       top_cities_cases_plot, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "species_comparison_case_count.png"), 
       species_cases_comparison_plot, width = 12, height = 8, dpi = 300)

# Save summary statistics
write_xlsx(analysis_summary, file.path(output_dir, "analysis_summary_statistics.xlsx"))
write.csv(analysis_summary, file.path(output_dir, "analysis_summary_statistics.csv"), row.names = FALSE)

# =============================================================================
# 8. Generate Analysis Report
# =============================================================================

cat("\n========== Comprehensive Malaria Spatial Analysis with Improved Legends Complete ==========\n")
cat("Analysis method: Geographic visualization with modern color scheme and improved legend display\n")

# Variable usage summary
cat("\nVariable usage:\n")
if (population_available) {
  cat("✓ Population data: Using 'population' variable, created incidence rate maps\n")
} else {
  cat("⚠ Population data: Not available, created case count maps only\n")
}

cat("\nLegend Improvements:\n")
cat("✓ Legend bar width: Increased from 15 to 25 for better readability\n")
cat("✓ Legend bar height: Increased from 0.5 to 0.8 for clearer display\n") 
cat("✓ Break points: Using 4 automatically optimized segment points\n")
cat("✓ Label alignment: Centered labels and titles for better appearance\n")
cat("✓ Color smoothness: Increased nbin to 100 for smoother gradients\n")
cat("✓ Margins: Added proper spacing around legends\n")

cat("\nColor Scheme:\n")
cat("✓ P. falciparum: Red gradient (#FEF0F0 to #922B21)\n")
cat("✓ P. vivax: Blue gradient (#EBF5FB to #1F4E79)\n") 
cat("✓ Total malaria: Green gradient (#EAFAF1 to #196F3D)\n")

cat("\nGenerated Files Summary:\n")
cat("CASE COUNT MAPS (with improved legends):\n")
cat("- Individual species case count maps (falciparum, vivax, total)\n")
cat("- Combined case count maps\n")
cat("- Case count comparison layouts\n")

if(population_available) {
  cat("INCIDENCE RATE MAPS (with improved legends):\n")
  cat("- Individual species incidence rate maps (per 1,000)\n")
  cat("- Combined incidence rate maps\n")
  cat("- Case count vs incidence rate comparison\n")
}

cat("DATA TABLES:\n")
cat("- City-level analysis data (with both counts and rates)\n")
cat("- Summary statistics\n")
cat("CHARTS:\n")
cat("- Top cities by case count\n")
cat("- Species comparison by case count\n")

if(population_available) {
  cat("- Top cities by incidence rate\n")
}

cat("\nAll files saved to:", output_dir, "\n")
cat("\nKey Findings:\n")
cat("- Total malaria cases:", format(analysis_summary$total_malaria_cases, big.mark = ","), "\n")
cat("- Mean cases per city:", round(analysis_summary$total_mean_cases, 1), "\n")

if(population_available) {
  cat("- Mean incidence rate:", round(analysis_summary$total_mean_incidence, 2), "per 1,000 people per year\n")
}

cat("- P. falciparum proportion:", round(analysis_summary$total_falciparum_cases / analysis_summary$total_malaria_cases * 100, 1), "%\n")
cat("- P. vivax proportion:", round(analysis_summary$total_vivax_cases / analysis_summary$total_malaria_cases * 100, 1), "%\n")

cat("\n========== Analysis Complete ==========\n")