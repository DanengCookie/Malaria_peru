# =============================================================================
# Falciparum and Vivax Malaria Time Series Trend Analysis
# Author: Data Analyst
# Date: September 2025
# Function: Separate analysis of falciparum malaria (Y0) and vivax malaria (Y1) time series trends
# =============================================================================

# Clear environment
rm(list = ls())

# Load necessary packages
library(readxl)       # Read Excel files
library(dplyr)        # Data processing
library(ggplot2)      # Data visualization
library(lubridate)    # Date processing
library(gridExtra)    # Combine charts
library(grid)         # For textGrob function
library(tidyr)        # For pivot_longer function
library(scales)       # Format labels
library(RColorBrewer) # Color schemes
library(viridis)      # Better color schemes
library(forecast)     # Time series analysis
library(zoo)          # Time series processing
library(data.table)   # Efficient data processing
library(plotly)       # Interactive charts (optional)
library(writexl)      # Write Excel files

# Set font for Windows systems
if (.Platform$OS.type == "windows") {
  windowsFonts(myFont = windowsFont("Arial"))
  par(family = "myFont")
}

# Create output directory
output_dir <- "C:/Users/zengz/Desktop/秘鲁疟疾/1时间序列/malaria_subspecies_timeseries_plots"
dir.create(output_dir, showWarnings = FALSE)

# =============================================================================
# 1. Data Reading and Preprocessing
# =============================================================================

cat("\n======= Reading and Preprocessing Malaria Subspecies Data =======\n")

# Read data
data <- read_excel("C:/Users/zengz/Desktop/秘鲁疟疾/merged_data.xlsx")

# Convert to data.table for improved processing efficiency
setDT(data)

# Check data structure
cat("Data dimensions:", dim(data), "\n")
cat("Column names:", colnames(data)[1:10], "...\n")
cat("Data overview:\n")
str(data[, 1:10])

# Data preprocessing
data[, year := as.integer(year)]
data[, week := as.integer(week)]
data[, Y0 := as.numeric(Y0)]  # Falciparum malaria
data[, Y1 := as.numeric(Y1)]  # Vivax malaria
data[, Y := as.numeric(Y)]    # Total malaria

# Handle missing values
data[is.na(Y0), Y0 := 0]
data[is.na(Y1), Y1 := 0]
data[is.na(Y), Y := 0]

cat("Falciparum malaria (Y0) statistics:\n")
print(summary(data$Y0))
cat("Vivax malaria (Y1) statistics:\n")
print(summary(data$Y1))
cat("Total malaria (Y) statistics:\n")
print(summary(data$Y))

# Create season variable based on Peru's climate
data[, season := case_when(
  week %in% c(1:17) ~ "Rainy_Season",      # Jan-Apr (rainy season)
  week %in% c(18:39) ~ "Dry_Season",       # May-Sep (dry season)
  week %in% c(40:52) ~ "Transition"        # Oct-Dec (transition period)
)]

# Create year-week variable for time series
data[, year_week := as.Date(paste(year, "01", "01", sep = "-")) + weeks(week - 1)]

cat("Data preprocessing completed!\n")
cat("Year range:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n")
cat("Total cities:", length(unique(data$cityid)), "\n")
cat("Total records:", nrow(data), "\n")

# =============================================================================
# 2. Annual Summary Analysis for Both Subspecies
# =============================================================================

cat("\n======= Annual Summary Analysis for Malaria Subspecies =======\n")

# Annual summary for both subspecies
yearly_summary <- data %>%
  group_by(year) %>%
  summarise(
    # Falciparum malaria statistics
    total_falciparum = sum(Y0, na.rm = TRUE),
    avg_falciparum_per_week = mean(Y0, na.rm = TRUE),
    # Vivax malaria statistics
    total_vivax = sum(Y1, na.rm = TRUE),
    avg_vivax_per_week = mean(Y1, na.rm = TRUE),
    # Total malaria statistics
    total_cases = sum(Y, na.rm = TRUE),
    avg_cases_per_week = mean(Y, na.rm = TRUE),
    # Proportion calculations
    falciparum_rate = total_falciparum / total_cases * 100,
    vivax_rate = total_vivax / total_cases * 100,
    # Other statistics
    cities_count = n_distinct(cityid),
    avg_temperature = mean(temperature, na.rm = TRUE),
    total_flood_events = sum(flood, na.rm = TRUE),
    flood_rate = mean(flood > 0, na.rm = TRUE) * 100,
    avg_precipitation = mean(total_precipitation_sum, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year)

cat("Annual summary statistics:\n")
print(yearly_summary)

# Calculate annual growth rates
yearly_summary <- yearly_summary %>%
  mutate(
    falciparum_growth_rate = (total_falciparum / lag(total_falciparum) - 1) * 100,
    vivax_growth_rate = (total_vivax / lag(total_vivax) - 1) * 100,
    total_growth_rate = (total_cases / lag(total_cases) - 1) * 100
  )

# =============================================================================
# 3. Weekly Summary Analysis for Both Subspecies
# =============================================================================

cat("\n======= Weekly Summary Analysis for Malaria Subspecies =======\n")

# Weekly summary (cross-year aggregation)
weekly_summary <- data %>%
  group_by(week) %>%
  summarise(
    # Falciparum malaria statistics
    total_falciparum = sum(Y0, na.rm = TRUE),
    avg_falciparum = mean(Y0, na.rm = TRUE),
    median_falciparum = median(Y0, na.rm = TRUE),
    falciparum_sd = sd(Y0, na.rm = TRUE),
    # Vivax malaria statistics
    total_vivax = sum(Y1, na.rm = TRUE),
    avg_vivax = mean(Y1, na.rm = TRUE),
    median_vivax = median(Y1, na.rm = TRUE),
    vivax_sd = sd(Y1, na.rm = TRUE),
    # Total malaria statistics
    total_cases = sum(Y, na.rm = TRUE),
    avg_cases = mean(Y, na.rm = TRUE),
    # Proportion calculations
    falciparum_week_rate = total_falciparum / total_cases * 100,
    vivax_week_rate = total_vivax / total_cases * 100,
    # Environmental factors
    avg_temperature = mean(temperature, na.rm = TRUE),
    total_flood_events = sum(flood, na.rm = TRUE),
    flood_rate = mean(flood > 0, na.rm = TRUE) * 100,
    avg_precipitation = mean(total_precipitation_sum, na.rm = TRUE),
    records_count = n(),
    .groups = 'drop'
  ) %>%
  arrange(week)

cat("Weekly summary statistics:\n")
print(weekly_summary)

# =============================================================================
# 4. Create Annual Trend Plots for Both Subspecies
# =============================================================================

cat("\n======= Creating Annual Trend Plots =======\n")

# Prepare data for plotting
yearly_long <- yearly_summary %>%
  select(year, total_falciparum, total_vivax, total_cases) %>%
  tidyr::pivot_longer(cols = c(total_falciparum, total_vivax), 
                      names_to = "malaria_type", 
                      values_to = "cases") %>%
  mutate(malaria_type = case_when(
    malaria_type == "total_falciparum" ~ "P. falciparum",
    malaria_type == "total_vivax" ~ "P. vivax"
  ))

# Plot 1: Annual cases comparison - P. falciparum vs P. vivax
p1 <- ggplot(yearly_long, aes(x = year, y = cases, color = malaria_type)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Annual Malaria Cases Trend by Species",
    subtitle = paste("Comparison: P. falciparum vs P. vivax (", min(yearly_summary$year), "-", max(yearly_summary$year), ")"),
    x = "Year",
    y = "Total Cases",
    color = "Malaria Species"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# Plot 2: Annual proportion stacked bar chart
yearly_prop <- yearly_summary %>%
  select(year, total_falciparum, total_vivax) %>%
  tidyr::pivot_longer(cols = c(total_falciparum, total_vivax), 
                      names_to = "malaria_type", 
                      values_to = "cases") %>%
  mutate(malaria_type = case_when(
    malaria_type == "total_falciparum" ~ "P. falciparum",
    malaria_type == "total_vivax" ~ "P. vivax"
  ))

p2 <- ggplot(yearly_prop, aes(x = year, y = cases, fill = malaria_type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Annual Malaria Cases Composition",
    subtitle = "Stacked view of P. falciparum vs P. vivax cases",
    x = "Year",
    y = "Total Cases",
    fill = "Malaria Species"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# Plot 3: Annual growth rate comparison
yearly_growth_long <- yearly_summary[-1, ] %>%
  select(year, falciparum_growth_rate, vivax_growth_rate) %>%
  tidyr::pivot_longer(cols = c(falciparum_growth_rate, vivax_growth_rate), 
                      names_to = "malaria_type", 
                      values_to = "growth_rate") %>%
  mutate(malaria_type = case_when(
    malaria_type == "falciparum_growth_rate" ~ "P. falciparum",
    malaria_type == "vivax_growth_rate" ~ "P. vivax"
  ))

p3 <- ggplot(yearly_growth_long, aes(x = year, y = growth_rate, fill = malaria_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#34495E") +
  scale_fill_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Annual Growth Rate Comparison",
    subtitle = "Year-over-year growth rate by malaria species",
    x = "Year",
    y = "Growth Rate (%)",
    fill = "Malaria Species"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# 5. Create Weekly Trend Plots for Both Subspecies
# =============================================================================

cat("\n======= Creating Weekly Trend Plots =======\n")

# Prepare weekly data for plotting
weekly_long <- weekly_summary %>%
  select(week, total_falciparum, total_vivax, avg_falciparum, avg_vivax) %>%
  tidyr::pivot_longer(cols = c(total_falciparum, total_vivax), 
                      names_to = "malaria_type", 
                      values_to = "total_cases") %>%
  mutate(malaria_type = case_when(
    malaria_type == "total_falciparum" ~ "P. falciparum",
    malaria_type == "total_vivax" ~ "P. vivax"
  ))

weekly_avg_long <- weekly_summary %>%
  select(week, avg_falciparum, avg_vivax) %>%
  tidyr::pivot_longer(cols = c(avg_falciparum, avg_vivax), 
                      names_to = "malaria_type", 
                      values_to = "avg_cases") %>%
  mutate(malaria_type = case_when(
    malaria_type == "avg_falciparum" ~ "P. falciparum",
    malaria_type == "avg_vivax" ~ "P. vivax"
  ))

# Plot 4: Weekly total cases comparison
p4 <- ggplot(weekly_long, aes(x = week, y = total_cases, fill = malaria_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Weekly Malaria Cases Distribution by Species",
    subtitle = "Total cases aggregated across all years",
    x = "Week",
    y = "Total Cases",
    fill = "Malaria Species"
  ) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# Plot 5: Weekly average cases trend lines
p5 <- ggplot(weekly_avg_long, aes(x = week, y = avg_cases, color = malaria_type)) +
  geom_line(size = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Weekly Average Malaria Cases Trend by Species",
    subtitle = "Shows seasonal patterns for each malaria species",
    x = "Week",
    y = "Average Cases",
    color = "Malaria Species"
  ) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# Plot 6: Weekly proportion stacked area chart
p6 <- ggplot(weekly_long, aes(x = week, y = total_cases, fill = malaria_type)) +
  geom_area(position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("P. falciparum" = "#E74C3C", "P. vivax" = "#3498DB")) +
  labs(
    title = "Weekly Malaria Cases Composition",
    subtitle = "Stacked area view showing dominance patterns by week",
    x = "Week",
    y = "Total Cases",
    fill = "Malaria Species"
  ) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# 6. Save Charts
# =============================================================================

cat("\n======= Saving Charts =======\n")

# Create combined annual charts
combined_annual <- grid.arrange(p1, p2, p3, ncol = 1, 
                                top = textGrob("Annual Malaria Species Analysis", 
                                               gp = gpar(fontsize = 18, fontface = "bold")))

# Create combined weekly charts
combined_weekly <- grid.arrange(p4, p5, p6, ncol = 1, 
                                top = textGrob("Weekly Malaria Species Analysis", 
                                               gp = gpar(fontsize = 18, fontface = "bold")))

# Save charts
ggsave(file.path(output_dir, "annual_malaria_species_analysis.png"), 
       combined_annual, width = 16, height = 18, dpi = 300)
ggsave(file.path(output_dir, "weekly_malaria_species_analysis.png"), 
       combined_weekly, width = 16, height = 18, dpi = 300)

# Save individual charts
ggsave(file.path(output_dir, "annual_comparison_lines.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "annual_composition_stacked.png"), p2, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "annual_growth_rates.png"), p3, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "weekly_comparison_bars.png"), p4, width = 14, height = 8, dpi = 300)
ggsave(file.path(output_dir, "weekly_trend_lines.png"), p5, width = 14, height = 8, dpi = 300)
ggsave(file.path(output_dir, "weekly_composition_area.png"), p6, width = 14, height = 8, dpi = 300)

# =============================================================================
# 7. Save Summary Statistics Results
# =============================================================================

cat("\n======= Saving Summary Statistics Results =======\n")

# Save summary data as Excel file
summary_data <- list(
  "Annual_Summary" = yearly_summary,
  "Weekly_Summary" = weekly_summary,
  "Annual_Long_Format" = yearly_long,
  "Weekly_Long_Format" = weekly_long
)

write_xlsx(summary_data, file.path(output_dir, "malaria_species_summary.xlsx"))

# Save as CSV files
write.csv(yearly_summary, file.path(output_dir, "yearly_species_summary.csv"), row.names = FALSE)
write.csv(weekly_summary, file.path(output_dir, "weekly_species_summary.csv"), row.names = FALSE)

# =============================================================================
# 8. Generate Analysis Report
# =============================================================================

cat("\n======= Generating Analysis Report =======\n")

# Calculate key statistical indicators
total_falciparum_all_years <- sum(yearly_summary$total_falciparum, na.rm = TRUE)
total_vivax_all_years <- sum(yearly_summary$total_vivax, na.rm = TRUE)
total_cases_all_years <- sum(yearly_summary$total_cases, na.rm = TRUE)

falciparum_proportion <- round(total_falciparum_all_years / total_cases_all_years * 100, 1)
vivax_proportion <- round(total_vivax_all_years / total_cases_all_years * 100, 1)

peak_falciparum_year <- yearly_summary$year[which.max(yearly_summary$total_falciparum)]
peak_vivax_year <- yearly_summary$year[which.max(yearly_summary$total_vivax)]

peak_falciparum_week <- weekly_summary$week[which.max(weekly_summary$total_falciparum)]
peak_vivax_week <- weekly_summary$week[which.max(weekly_summary$total_vivax)]

# Generate text report
report <- paste0(
  "Plasmodium falciparum and Plasmodium vivax Time Series Analysis Report\n",
  "=====================================================================\n\n",
  "Data Overview:\n",
  "- Analysis period: ", min(yearly_summary$year), " - ", max(yearly_summary$year), "\n",
  "- Total cases: ", format(total_cases_all_years, big.mark = ","), "\n",
  "- Total P. falciparum: ", format(total_falciparum_all_years, big.mark = ","), 
  " (", falciparum_proportion, "%)\n",
  "- Total P. vivax: ", format(total_vivax_all_years, big.mark = ","), 
  " (", vivax_proportion, "%)\n",
  "- Cities analyzed: ", length(unique(data$cityid)), "\n\n",
  
  "Key Findings:\n",
  "- P. falciparum peak year: ", peak_falciparum_year, 
  " (", format(max(yearly_summary$total_falciparum), big.mark = ","), " cases)\n",
  "- P. vivax peak year: ", peak_vivax_year, 
  " (", format(max(yearly_summary$total_vivax), big.mark = ","), " cases)\n",
  "- P. falciparum peak week: ", peak_falciparum_week, "\n",
  "- P. vivax peak week: ", peak_vivax_week, "\n\n",
  
  "Trend Characteristics:\n"
)

if (nrow(yearly_summary) > 1) {
  recent_falciparum_trend <- ifelse(yearly_summary$falciparum_growth_rate[nrow(yearly_summary)] > 0, "Increasing", "Decreasing")
  recent_vivax_trend <- ifelse(yearly_summary$vivax_growth_rate[nrow(yearly_summary)] > 0, "Increasing", "Decreasing")
  
  report <- paste0(report,
                   "- Recent P. falciparum trend: ", recent_falciparum_trend,
                   " (", round(yearly_summary$falciparum_growth_rate[nrow(yearly_summary)], 1), "%)\n",
                   "- Recent P. vivax trend: ", recent_vivax_trend,
                   " (", round(yearly_summary$vivax_growth_rate[nrow(yearly_summary)], 1), "%)\n\n"
  )
}

report <- paste0(report,
                 "Generated Files:\n",
                 "1. Annual malaria species trend analysis\n",
                 "2. Weekly malaria species trend analysis\n",
                 "3. Summary statistics tables\n",
                 "4. Individual charts\n\n",
                 
                 "Files saved to: ", output_dir, "\n"
)

# Save report
writeLines(report, file.path(output_dir, "species_analysis_report.txt"))

# Console output report
cat(report)

cat("\n======= Plasmodium falciparum and Plasmodium vivax Time Series Analysis Completed! =======\n")
cat("Charts saved to:", output_dir, "\n")
cat("Please check the generated charts to understand temporal trends and seasonal patterns of malaria species.\n")