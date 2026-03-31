library(tidyverse)
library(lubridate)
library(dplyr)

# ---------------------------------------------------------------------------------------------------------------------------
# Compare SWOT PIXC & FODAR 5m DEM heights: Node polygon bins
# ---------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------
# read in SWOT data
SWOT_df <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixc/PGD0/PIXC_node_polygon_watermask.csv')
SWOT_df$SWOTFileDate <- as.character(SWOT_df$SWOTFileDate)

# filter to single date for comparisons
# manually adjust date
SWOT_df_filtered <- SWOT_df %>%
  filter(as.character(SWOTFileDate) == '2025-03-24 18:26:59') 
# datetime options:
# 2025-03-22 03:12:47
# 2025-03-24 18:26:59
# 2025-04-03 16:49:14

# add SWOT_ prefix to columns
SWOT_df_filtered <- SWOT_df_filtered %>%
  rename_with(.fn = ~ paste0("SWOT_", .), .cols = -node_id)

# ---------------------------------------------------------------------------------------------------------------------------
# read in FODAR data
FODAR_df <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/FODAR/SWOT_CRS/fodar_node_polygon_watermask.csv')

# filter to single date for comparisons
# manually adjust date
FODAR_df_filtered <- FODAR_df %>%
  filter(FODARFileDate == 20250323)
# date options:
# 20250323
# 20250403

# add FODAR_ prefix to columns
FODAR_df_filtered <- FODAR_df_filtered %>%
  rename_with(.fn = ~ paste0("FODAR_", .), .cols = -node_id)

# ---------------------------------------------------------------------------------------------------------------------------
# match SWOT & FODAR obs by node id

# join SWORD to add dist_out variable
SWORD_nodes <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/TN_AOI/TN_ice_domain_nodes_v17b.csv')

# join SWOT & FODAR
SWOT_FODAR_df <- inner_join(FODAR_df_filtered, SWOT_df_filtered, by = "node_id") %>%
  dplyr::select(-FODAR_reach_id, -SWOT_reach_id)
SWOT_FODAR_df <- inner_join(SWOT_FODAR_df, SWORD_nodes, by = "node_id")

# ---------------------------------------------------------------------------------------------------------------------------
# MEAN comparison

# Residuals: FODAR - SWOT
SWOT_FODAR_df <- SWOT_FODAR_df %>%
  mutate(residuals_mean_m = FODAR_elev_mean_m - SWOT_elev_mean_m)

# 68th & 50th percentile error
percentile_68_error_mean <- quantile(abs(SWOT_FODAR_df$residuals_mean_m), 0.68, na.rm = TRUE)
percentile_50_error_mean <- quantile(abs(SWOT_FODAR_df$residuals_mean_m), 0.50, na.rm = TRUE)

print(paste("Mean comparison 68th Percentile Error:", percentile_68_error_mean))
print(paste("Mean comparison 50th Percentile Error:", percentile_50_error_mean))

# RMSE
rmse_mean <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_mean_m - SWOT_FODAR_df$SWOT_elev_mean_m)^2, na.rm = TRUE))

# correlation test
cor_test_mean <- cor.test(
  SWOT_FODAR_df$SWOT_elev_mean_m,
  SWOT_FODAR_df$FODAR_elev_mean_m
)

r_value_mean <- cor_test_mean$estimate
p_value_mean <- cor_test_mean$p.value

# Bias correction for mean comparison
bias_mean <- median(SWOT_FODAR_df$residuals_mean_m, na.rm = TRUE)

SWOT_FODAR_df <- SWOT_FODAR_df %>%
  mutate(
    FODAR_elev_mean_nobias_m = FODAR_elev_mean_m - bias_mean,
    residuals_mean_nobias_m = FODAR_elev_mean_nobias_m - SWOT_elev_mean_m
  )

percentile_68_error_mean_nobias <- quantile(abs(SWOT_FODAR_df$residuals_mean_nobias_m), 0.68, na.rm = TRUE)
percentile_50_error_mean_nobias <- quantile(abs(SWOT_FODAR_df$residuals_mean_nobias_m), 0.50, na.rm = TRUE)

print(paste("Mean comparison 68th Percentile Error Without Bias:", percentile_68_error_mean_nobias))
print(paste("Mean comparison 50th Percentile Error Without Bias:", percentile_50_error_mean_nobias))

rmse_mean_nobias <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_mean_nobias_m - SWOT_FODAR_df$SWOT_elev_mean_m)^2, na.rm = TRUE))

cor_test_mean_nobias <- cor.test(
  SWOT_FODAR_df$SWOT_elev_mean_m,
  SWOT_FODAR_df$FODAR_elev_mean_nobias_m
)

r_value_mean_nobias <- cor_test_mean_nobias$estimate
p_value_mean_nobias <- cor_test_mean_nobias$p.value

# ---------------------------------------------------------------------------------------------------------------------------
# 10TH PERCENTILE comparison

# Residuals: FODAR - SWOT
SWOT_FODAR_df <- SWOT_FODAR_df %>%
  mutate(
    residuals_10pct_m = FODAR_elev_10percentile_m - SWOT_elev_10percentile_m
  )

# 68th & 50th percentile error
percentile_68_error_10pct <- quantile(abs(SWOT_FODAR_df$residuals_10pct_m), 0.68, na.rm = TRUE)
percentile_50_error_10pct <- quantile(abs(SWOT_FODAR_df$residuals_10pct_m), 0.50, na.rm = TRUE)

print(paste("10th percentile comparison 68th Percentile Error:", percentile_68_error_10pct))
print(paste("10th percentile comparison 50th Percentile Error:", percentile_50_error_10pct))

# RMSE
rmse_10pct <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_10percentile_m - SWOT_FODAR_df$SWOT_elev_10percentile_m)^2, na.rm = TRUE))

# correlation test
cor_test_10pct <- cor.test(
  SWOT_FODAR_df$SWOT_elev_10percentile_m,
  SWOT_FODAR_df$FODAR_elev_10percentile_m
)

r_value_10pct <- cor_test_10pct$estimate
p_value_10pct <- cor_test_10pct$p.value

# Bias correction for 10th percentile comparison
bias_10pct <- median(SWOT_FODAR_df$residuals_10pct_m, na.rm = TRUE)

SWOT_FODAR_df <- SWOT_FODAR_df %>%
  mutate(
    FODAR_elev_10percentile_nobias_m = FODAR_elev_10percentile_m - bias_10pct,
    residuals_10pct_nobias_m = FODAR_elev_10percentile_nobias_m - SWOT_elev_10percentile_m
  )

percentile_68_error_10pct_nobias <- quantile(abs(SWOT_FODAR_df$residuals_10pct_nobias_m), 0.68, na.rm = TRUE)
percentile_50_error_10pct_nobias <- quantile(abs(SWOT_FODAR_df$residuals_10pct_nobias_m), 0.50, na.rm = TRUE)

print(paste("10th percentile comparison 68th Percentile Error Without Bias:", percentile_68_error_10pct_nobias))
print(paste("10th percentile comparison 50th Percentile Error Without Bias:", percentile_50_error_10pct_nobias))

rmse_10pct_nobias <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m - SWOT_FODAR_df$SWOT_elev_10percentile_m)^2, na.rm = TRUE))

cor_test_10pct_nobias <- cor.test(
  SWOT_FODAR_df$SWOT_elev_10percentile_m,
  SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m
)

r_value_10pct_nobias <- cor_test_10pct_nobias$estimate
p_value_10pct_nobias <- cor_test_10pct_nobias$p.value

# ---------------------------------------------------------------------------------------------------------------------------
# WRITE FULL DATA FRAME TO CSV

write.csv(
  SWOT_FODAR_df,
  file = "/Users/camryn/Documents/UNC/Ice_caval/Tanana/CalVal_dataframes/node_SWOT_FODAR_03242025.csv",
  row.names = FALSE)


# ---------------------------------------------------------------------------------------------------------------------------
# PLOTS

# correlation test
cor_test_nobias <- cor.test(SWOT_FODAR_df$SWOT_elev_10percentile_m, SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m)

# Extract r and p-value
r_value_nobias <- cor_test_nobias$estimate # Pearson correlation coefficient
p_value_nobias <- cor_test_nobias$p.value # 

# plot SWOT vs FODAR elevation
ggplot(SWOT_FODAR_df, aes(x = FODAR_elev_10percentile_nobias_m, y = SWOT_elev_10percentile_m)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("FODAR height (m)") +
  ylab("SWOT height (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m, na.rm = TRUE), 
           y = max(SWOT_FODAR_df$SWOT_elev_10percentile_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nRMSE = ", signif(rmse_nobias, 3),
                          "\nn = ", nrow(SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# Create a long format data frame for plotting
plot_df <- SWOT_FODAR_df %>%
  dplyr::select(dist_out, SWOT_elev_10percentile_m, FODAR_elev_10percentile_nobias_m) %>%
  pivot_longer(cols = c(SWOT_elev_10percentile_m, FODAR_elev_10percentile_nobias_m),
               names_to = "source", values_to = "elevation_m") %>%
  mutate(source = recode(source,
                         "SWOT_elev_10percentile_m" = "SWOT",
                         "FODAR_elev_10percentile_nobias_m" = "FODAR"))

# Plot with legend
ggplot(plot_df) +
  geom_point(aes(x = dist_out / 1000, y = elevation_m, color = source), size = 4) +
  scale_color_manual(values = c("SWOT" = "#00429D", "FODAR" = "#9EBCD8")) +
  xlab("Distance to outlet (km)") +
  ylab("Elevation (m)") +
  theme_minimal(base_size = 30) +
  labs(color = "Data type")

# dist to outlet
ggplot(SWOT_FODAR_df) +
  geom_point(aes(x = dist_out / 1000, y = residuals_nobias), size = 4, color = "#00429D") + # 
  xlab("Distance to outlet (km)") +
  ylab("residuals (m)") +
  theme_minimal(base_size = 30) 

# CDF plot
ggplot(SWOT_FODAR_df, aes(x = abs(SWOT_elev_10percentile_m - FODAR_elev_10percentile_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1.6) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "| SWOT Height - FODAR Height | (m)", y = "Cumulative Probability", title = "Relative 10%ile Height Differences") +
  annotate("text", x = 0.75, y = 0.71, label = paste("| 68%ile |:", round(percentile_68_error_nobias, 4), "m"), color = "#222222", size = 7) +
  annotate("text", x = 0.75, y = 0.53, label = paste("| 50%ile |:", round(percentile_50_error_nobias, 3), "m"), color = "#222222", size = 7) +
  theme_minimal(base_size = 22) +
  coord_cartesian(xlim = c(0, 1))





# ---------------------------------------------------------------------------------------------------------------------------
# All days stats & data viz

# Set working directory to the folder with SWOT_FODAR_dfs
wd <- "/Users/camryn/Documents/UNC/Ice_caval/Tanana/CalVal_dataframes"
setwd(wd)

# List of files to join that start with node_
csv_files <- list.files(wd, pattern = "node_*", full.names = TRUE)

# Merge all files into a combined dataframe
data_list <- lapply(seq_along(csv_files), function(i) {
  df <- read.csv(csv_files[i])
  return(df)
})

combined_SWOT_FODAR_df <- bind_rows(data_list)

# correlation test
cor_test_nobias <- cor.test(combined_SWOT_FODAR_df$SWOT_elev_mean_m, combined_SWOT_FODAR_df$FODAR_elev_mean_nobias_m)

# Extract r and p-value
r_value_nobias <- cor_test_nobias$estimate # Pearson correlation coefficient
p_value_nobias <- cor_test_nobias$p.value # 

# plot SWOT vs FODAR elevation
ggplot(combined_SWOT_FODAR_df, aes(x = FODAR_elev_mean_nobias_m, y = SWOT_elev_mean_m)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("FODAR elevation (m)") +
  ylab("SWOT elevation (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(combined_SWOT_FODAR_df$FODAR_elev_mean_nobias_m, na.rm = TRUE), 
           y = max(combined_SWOT_FODAR_df$SWOT_elev_mean_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value_nobias, 4), "\np value = ", signif(p_value_nobias, 3),
                          "\nn = ", nrow(combined_SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# Calculate the 68th & 50th percentile error
percentile_68_error_nobias <- quantile(abs(combined_SWOT_FODAR_df$residuals_10pct_nobias_m), 0.68, na.rm=TRUE)
percentile_50_error_nobias <- quantile(abs(combined_SWOT_FODAR_df$residuals_10pct_nobias_m), 0.50, na.rm=TRUE)

# CDF plot
ggplot(combined_SWOT_FODAR_df, aes(x = abs(residuals_10pct_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Elev - FODAR Elev (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Elevation") +
  annotate("text", x = 4, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)), color = "#222222", size = 6) +
  annotate("text", x = 4, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) +
  coord_cartesian(xlim = c(0, 5))


plot_df <- combined_SWOT_FODAR_df %>%
  mutate(
    group = case_when(
      FODAR_FODARFileDate == 20250323 ~ "2025-03-23",
      FODAR_FODARFileDate == 20250403 ~ "2025-04-03",
      TRUE ~ NA_character_
    )
  )

ggplot() +
  # --- Combined (ALL data) ---
  stat_ecdf(
    data = combined_SWOT_FODAR_df,
    aes(x = abs(residuals_10pct_nobias_m)),
    color = "darkblue",
    size = 1.2
  ) +
  
  # --- Individual dates ---
  stat_ecdf(
    data = plot_df %>% filter(!is.na(group)),
    aes(x = abs(residuals_10pct_nobias_m), color = group),
    size = 1
  ) +
  
  scale_color_manual(
    values = c(
      "2025-03-23" = "#E66100",
      "2025-04-03" = "#5D3A9B"
    ),
    name = "FODAR Date"
  ) +
  
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  
  labs(
    x = "SWOT Elev - FODAR Elev (m)",
    y = "Cumulative Probability",
    title = "CDF of SWOT - FODAR Elevation"
  ) +
  
  # --- annotations tied to COMBINED curve ---
  annotate(
    "text",
    x = 4, y = 0.71,
    label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)),
    color = "darkblue", size = 6
  ) +
  annotate(
    "text",
    x = 4, y = 0.53,
    label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)),
    color = "darkblue", size = 6
  ) +
  
  theme_minimal(base_size = 20) +
  coord_cartesian(xlim = c(0, 5))




# histogram
ggplot(combined_SWOT_FODAR_df,
       aes(x = residuals_10pct_nobias_m,
           fill = factor(FODAR_FODARFileDate))) +
  geom_histogram(bins = 40, position = "stack",
    # color = "black"   # optional: outlines for clarity
  ) +
  scale_fill_manual(values = c(
      "20250323" = "#E66100",
      "20250403" = "#5D3A9B"),
    name = "FODAR Date",
    labels = c("2025-03-23", "2025-04-03")) +
  labs(
    x = "Residuals (FODAR - SWOT, m)",
    y = "Count",
    title = "Stacked Histogram of Residuals (10th Percentile, No Bias)") +
  theme_minimal(base_size = 20)
