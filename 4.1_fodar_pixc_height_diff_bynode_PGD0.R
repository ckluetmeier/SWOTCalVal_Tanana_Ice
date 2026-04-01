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

# ******* Fig 1 ***********
# dist to outlet colored by elevation
ggplot(SWOT_FODAR_df) +
  geom_point(aes(x = dist_out / 1000, y = SWOT_elev_10percentile_m, color = SWOT_elev_10percentile_m), size = 3) +
  scale_color_distiller(palette = "GnBu", direction = 1, name = "Height (m)") +
  xlab("Distance to outlet (km)") +
  ylab("SWOT river ice height (m)") +
  theme_minimal(base_size = 30) +
  theme(legend.position = "none")

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
cor_test_nobias <- cor.test(combined_SWOT_FODAR_df$SWOT_elev_10percentile_m, combined_SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m)

# Extract r and p-value
r_value_nobias <- cor_test_nobias$estimate # Pearson correlation coefficient
p_value_nobias <- cor_test_nobias$p.value # 

# plot SWOT vs FODAR height
ggplot(combined_SWOT_FODAR_df, aes(x = FODAR_elev_10percentile_nobias_m, y = SWOT_elev_10percentile_m)) +
  geom_point(size = 4, aes(color = factor(FODAR_FODARFileDate))) +
  scale_color_manual(
    values = c("20250323" = "#7571AE", "20250403" = "#4C9C7B"),
    labels = c("20250323" = "2025-03-24", "20250403" = "2025-04-03"),
    name = "FODAR Date"
  ) +
  xlab("DEM height (m)") +
  ylab("SWOT height (m)") +
  ggtitle("(b) SWOT vs. DEM Heights") +
  theme_minimal(base_size = 25) +
  geom_abline(linetype = "dashed", color = "gray") +
  annotate("text", x = min(combined_SWOT_FODAR_df$FODAR_elev_mean_nobias_m, na.rm = TRUE), 
           y = max(combined_SWOT_FODAR_df$SWOT_elev_mean_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value_nobias, 4), "\np value = ", round(p_value_nobias, 4),
                          "\nn = ", nrow(combined_SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)



# CDF plot by day
# Pre-compute stats for each group
n_combined <- nrow(combined_SWOT_FODAR_df)
q68_combined <- round(quantile(abs(combined_SWOT_FODAR_df$residuals_10pct_nobias_m), 0.68, na.rm = TRUE), 3)
q50_combined <- round(quantile(abs(combined_SWOT_FODAR_df$residuals_10pct_nobias_m), 0.50, na.rm = TRUE), 3)

df_323 <- combined_SWOT_FODAR_df %>% filter(FODAR_FODARFileDate == 20250323)
n_323 <- nrow(df_323)
q68_323 <- round(quantile(abs(df_323$residuals_10pct_nobias_m), 0.68, na.rm = TRUE), 3)
q50_323 <- round(quantile(abs(df_323$residuals_10pct_nobias_m), 0.50, na.rm = TRUE), 3)

df_403 <- combined_SWOT_FODAR_df %>% filter(FODAR_FODARFileDate == 20250403)
n_403 <- nrow(df_403)
q68_403 <- round(quantile(abs(df_403$residuals_10pct_nobias_m), 0.68, na.rm = TRUE), 3)
q50_403 <- round(quantile(abs(df_403$residuals_10pct_nobias_m), 0.50, na.rm = TRUE), 3)


plot_df <- combined_SWOT_FODAR_df %>%
  mutate(group = case_when(
    FODAR_FODARFileDate == 20250323 ~ "2025-03-24",
    FODAR_FODARFileDate == 20250403 ~ "2025-04-03",
    TRUE ~ NA_character_))

ggplot() +

  # --- Individual dates ---
  stat_ecdf(
    data = plot_df %>% filter(!is.na(group)),
    aes(x = abs(residuals_10pct_nobias_m), color = group),
    linetype = "longdash",
    size = 1
  ) +
  
  # --- Combined (ALL data) ---
  stat_ecdf(
    data = combined_SWOT_FODAR_df,
    aes(x = abs(residuals_10pct_nobias_m)),
    color = "black",
    size = 1.4
  ) +
  
  scale_color_manual(
    values = c(
      "2025-03-23" = "#7571AE",
      "2025-04-03" = "#4C9C7B"
    )
  ) +
  
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  
  labs(
    x = "| SWOT - DEM height | (m)",
    y = "Cumulative Percentage (%)",
    title = "(a) CDF of Relative Height Error"
  ) +
  
  # --- Annotations: horizontal reference line labels ---
  # annotate("text", x = 2, y = 0.71,
  #          label = paste("|68% diff|:", round(q68_combined, 3)), size = 6) +
  # annotate("text", x = 2, y = 0.53,
  #          label = paste("|50% diff|:", round(q50_combined, 3)), size = 6) +
  
  # --- Legend annotations: Combined ---
  annotate("text", x = Inf, y = 0.14, hjust = 1, vjust = 0,
           label = paste0("Combined: n = ", n_combined,
                          ",  |68%ile| = ", q68_combined,
                          " m,  |50%ile| = ", q50_combined, " m"),
           color = "black", size = 5.5) +
  
  # --- Legend annotations: 2025-03-23 ---
  annotate("text", x = Inf, y = 0.08, hjust = 1, vjust = 0,
           label = paste0("2025-03-24: n = ", n_323,
                          ",  |68%ile| = ", q68_323,
                          " m,  |50%ile| = ", q50_323, " m"),
           color = "#7571AE", size = 5.4) +
  
  # --- Legend annotations: 2025-04-03 ---
  annotate("text", x = Inf, y = 0.02, hjust = 1, vjust = 0,
           label = paste0("2025-04-03: n = ", n_403,
                          ",  |68%ile| = ", q68_403,
                          " m,  |50%ile| = ", q50_403, " m"),
           color = "#4C9C7B", size = 5.4) +
  
  theme_minimal(base_size = 25) +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 5))




# histogram
ggplot(combined_SWOT_FODAR_df,
       aes(x = residuals_10pct_nobias_m,
           fill = factor(FODAR_FODARFileDate))) +
  geom_histogram(bins = 40, position = "stack",
    # color = "black"   # optional: outlines for clarity
  ) +
  scale_fill_manual(values = c(
      "20250323" = "#E69F00",
      "20250403" = "#56B4E9"),
    name = "FODAR Date",
    labels = c("2025-03-23", "2025-04-03")) +
  labs(
    x = "Residuals (FODAR - SWOT, m)",
    y = "Count",
    title = "Histogram of Height Error (10th Percentile, No Bias)") +
  theme_minimal(base_size = 20)
