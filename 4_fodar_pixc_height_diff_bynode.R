library(tidyverse)
library(lubridate)
library(dplyr)

# ---------------------------------------------------------------------------------------------------------------------------
# Compare SWOT PIXC & FODAR 5m DEM heights: Node polygon bins
# ---------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------
# read in SWOT data
SWOT_df <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixc/UTM6N_tidecorrections/summaryStats_pixc_node_polygon_ellipsoid.csv')
SWOT_df$SWOTFileDate <- as.character(SWOT_df$SWOTFileDate)

# filter to single date for comparisons
# manually adjust date
SWOT_df_filtered <- SWOT_df %>%
  filter(as.character(SWOTFileDate) == '2025-03-22 03:12:47') 
# datetime options:
# 2025-03-22 03:12:47
# 2025-03-24 18:26:59
# 2025-04-03 16:49:14

# add SWOT_ prefix to columns
SWOT_df_filtered <- SWOT_df_filtered %>%
  rename_with(.fn = ~ paste0("SWOT_", .), .cols = -node_id)

# ---------------------------------------------------------------------------------------------------------------------------
# read in FODAR data
FODAR_df <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/FODAR/summaryStats_fodar_node_polygon_ellipsoid.csv')

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
SWORD_nodes <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWORD_clipped_TN_domain/TN_domain_SWORD_nodes_v16.csv')

# fix a dist_out topology bug in SWORD v16
SWORD_nodes$dist_out_fix <- ifelse(SWORD_nodes$dist_out < 1600000, 
                               SWORD_nodes$dist_out + 151198, 
                               SWORD_nodes$dist_out)

# join SWOT & FODAR
SWOT_FODAR_df <- inner_join(FODAR_df_filtered, SWOT_df_filtered, by = "node_id") %>%
  dplyr::select(-FODAR_reach_id, -SWOT_reach_id)
SWOT_FODAR_df <- inner_join(SWOT_FODAR_df, SWORD_nodes, by = "node_id")


# ---------------------------------------------------------------------------------------------------------------------------
# Summary stats for mean height difference

# Calculate the height diff FODAR - SWOT (residuals)
SWOT_FODAR_df$residuals = SWOT_FODAR_df$FODAR_elev_mean_m - SWOT_FODAR_df$SWOT_elev_mean_m

# Calculate 68th & 50th percentile error
percentile_68_error <- quantile(abs(SWOT_FODAR_df$residuals), 0.68, na.rm=TRUE)
percentile_50_error <- quantile(abs(SWOT_FODAR_df$residuals), 0.50, na.rm=TRUE)

# #print the result
# print(paste("68th Percentile Error:", percentile_68_error))
# print(paste("50th Percentile Error:", percentile_50_error))

#RMSE
rmse <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_mean_m - SWOT_FODAR_df$SWOT_elev_mean_m)^2))

# correlation test
cor_test <- cor.test(SWOT_FODAR_df$SWOT_elev_mean_m, SWOT_FODAR_df$FODAR_elev_mean_m)
# Extract r and p-value
r_value <- cor_test$estimate # Pearson correlation coefficient
p_value <- cor_test$p.value # highly statistically significant is P < 0.001

# ---------------------------------------------------------------------------------------------------------------------------
# data viz

# plot residuals vs FODAR std
ggplot(SWOT_FODAR_df, aes(x = residuals, y = FODAR_elev_std_m, color = factor(reach_id))) +
  geom_point(size = 3.5) +
  xlab("FODAR - SWOT residuals (m)") +
  ylab("FODAR std") +
  theme_minimal(base_size = 30)  +
  labs(color = "Reach ID") 

# plot SWOT vs FODAR height
ggplot(SWOT_FODAR_df, aes(x = FODAR_elev_mean_m, y = SWOT_elev_mean_m)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("FODAR height (m)") +
  ylab("SWOT height (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(SWOT_FODAR_df$FODAR_elev_mean_m, na.rm = TRUE), 
           y = max(SWOT_FODAR_df$SWOT_elev_mean_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nRMSE = ", signif(rmse, 3),
                          "\nn = ", nrow(SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# residuals over dist to outlet
ggplot(SWOT_FODAR_df, aes(x = dist_out_fix/1000, y = residuals)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("dist to outlet (km)") +
  ylab("residuals (m)") +
  theme_minimal(base_size = 30) 

# create an expanded df for swot vs fodar over dist_out_fix
plot_df <- SWOT_FODAR_df %>%
  dplyr::select(dist_out_fix, SWOT_elev_mean_m, FODAR_elev_mean_m) %>%
  pivot_longer(cols = c(SWOT_elev_mean_m, FODAR_elev_mean_m),
               names_to = "source", values_to = "elevation_m") %>%
  mutate(source = recode(source,
                         "SWOT_elev_mean_m" = "SWOT",
                         "FODAR_elev_mean_m" = "FODAR"))

# swot vs fodar over dist_out_fix
ggplot(plot_df) +
  geom_point(aes(x = dist_out_fix / 1000, y = elevation_m, color = source), size = 4) +
  scale_color_manual(values = c("SWOT" = "#00429D", "FODAR" = "#9EBCD8")) +
  xlab("Distance to outlet (km)") +
  ylab("Height (m)") +
  theme_minimal(base_size = 30) +
  labs(color = "Data type")


# CDF plot
ggplot(SWOT_FODAR_df, aes(x = abs(SWOT_elev_mean_m - FODAR_elev_mean_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Height - FODAR Height (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Height") +
  annotate("text", x = 2.5, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error, 4)), color = "#222222", size = 6) +
  annotate("text", x = 2.5, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 

# ---------------------------------------------------------------------------------------------------------------------------
# remove bias from FODAR data

bias <- median(SWOT_FODAR_df$residuals, na.rm = TRUE)
SWOT_FODAR_df$FODAR_elev_mean_nobias_m = SWOT_FODAR_df$FODAR_elev_mean_m - bias

# Calculate the height diff FODAR - SWOT (residuals)
SWOT_FODAR_df$residuals_nobias = SWOT_FODAR_df$FODAR_elev_mean_nobias_m - SWOT_FODAR_df$SWOT_elev_mean_m

# Calculate the 68th & 50th percentile error
percentile_68_error_nobias <- quantile(abs(SWOT_FODAR_df$residuals_nobias), 0.68, na.rm=TRUE)
percentile_50_error_nobias <- quantile(abs(SWOT_FODAR_df$residuals_nobias), 0.50, na.rm=TRUE)

# print the result
# print(paste("68th Percentile Error Without Bias:", percentile_68_error_nobias))
# print(paste("50th Percentile Error Without Bias:", percentile_50_error_nobias))

# subset for saving to CSV
save_to_csv <- SWOT_FODAR_df %>%
  dplyr::select(node_id, reach_id, residuals, residuals_nobias, FODAR_FODARFileDate, SWOT_SWOTFileDate, FODAR_elev_mean_m, SWOT_elev_mean_m, FODAR_elev_mean_nobias_m, SWOT_elev_std_m, FODAR_elev_std_m, dist_out_fix)

# save to csv
# write.csv(save_to_csv, file = '/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT_FODAR_df_20250322.csv', row.names = FALSE)

# RMSE
rmse_nobias <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_mean_nobias_m - SWOT_FODAR_df$SWOT_elev_mean_m)^2))

# correlation test
cor_test_nobias <- cor.test(SWOT_FODAR_df$SWOT_elev_mean_m, SWOT_FODAR_df$FODAR_elev_mean_nobias_m)

# Extract r and p-value
r_value_nobias <- cor_test_nobias$estimate # Pearson correlation coefficient
p_value_nobias <- cor_test_nobias$p.value # 

# plot SWOT vs FODAR elevation
ggplot(SWOT_FODAR_df, aes(x = FODAR_elev_mean_nobias_m, y = SWOT_elev_mean_m)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("FODAR height (m)") +
  ylab("SWOT height (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(SWOT_FODAR_df$FODAR_elev_mean_nobias_m, na.rm = TRUE), 
           y = max(SWOT_FODAR_df$SWOT_elev_mean_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nRMSE = ", signif(rmse_nobias, 3),
                          "\nn = ", nrow(SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# create an expanded df for swot vs fodar over dist_out_fix
plot_df <- SWOT_FODAR_df %>%
  dplyr::select(dist_out_fix, SWOT_elev_mean_m, FODAR_elev_mean_nobias_m) %>%
  pivot_longer(cols = c(SWOT_elev_mean_m, FODAR_elev_mean_nobias_m),
               names_to = "source", values_to = "elevation_m") %>%
  mutate(source = recode(source,
                         "SWOT_elev_mean_m" = "SWOT",
                         "FODAR_elev_mean_nobias_m" = "FODAR"))

# swot vs fodar over dist_out_fix
ggplot(plot_df) +
  geom_point(aes(x = dist_out_fix / 1000, y = elevation_m, color = source), size = 4) +
  scale_color_manual(values = c("SWOT" = "#00429D", "FODAR" = "#9EBCD8")) +
  xlab("Distance to outlet (km)") +
  ylab("Elevation (m)") +
  theme_minimal(base_size = 30) +
  labs(color = "Data type")


# CDF plot
ggplot(SWOT_FODAR_df, aes(x = abs(SWOT_elev_mean_m - FODAR_elev_mean_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Height - FODAR Height (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Height") +
  annotate("text", x = 2, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)), color = "#222222", size = 6) +
  annotate("text", x = 2, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 

# ---------------------------------------------------------------------------------------------------------------------------
# Summary stats using tenth percentile value

# Calculate the elev diff FODAR - SWOT (residuals)
SWOT_FODAR_df$residuals = SWOT_FODAR_df$FODAR_elev_10percentile_m - SWOT_FODAR_df$SWOT_elev_10percentile_m


# Calculate the 68th percentile error
percentile_68_error <- quantile(abs(SWOT_FODAR_df$residuals), 0.68, na.rm=TRUE)
percentile_50_error <- quantile(abs(SWOT_FODAR_df$residuals), 0.50, na.rm=TRUE)

# #print the result
# print(paste("68th Percentile Error:", percentile_68_error))
# print(paste("50th Percentile Error:", percentile_50_error))

#RMSE
rmse <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_10percentile_m - SWOT_FODAR_df$SWOT_elev_10percentile_m)^2))
#RMSE >= MAE, MAE is similar to 50th quantile error

# correlation test
cor_test <- cor.test(SWOT_FODAR_df$SWOT_elev_10percentile_m, SWOT_FODAR_df$FODAR_elev_10percentile_m)

# Extract r and p-value
r_value <- cor_test$estimate # Pearson correlation coefficient
p_value <- cor_test$p.value # highly statistically significant is P < 0.001

# ---------------------------------------------------------------------------------------------------------------------------
# data viz

# plot residuals vs std
ggplot(SWOT_FODAR_df, aes(x = residuals, y = FODAR_elev_std_m, color = factor(reach_id))) +
  geom_point(size = 3.5) +
  xlab("FODAR - SWOT residuals (m)") +
  ylab("FODAR std") +
  theme_minimal(base_size = 30)  +
  labs(color = "Reach ID") 

# plot SWOT vs FODAR elevation
ggplot(SWOT_FODAR_df, aes(x = FODAR_elev_10percentile_m, y = SWOT_elev_10percentile_m)) +
  geom_point(size = 4, color = "darkblue") +
  xlab("FODAR height (m)") +
  ylab("SWOT height (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(SWOT_FODAR_df$FODAR_elev_10percentile_m, na.rm = TRUE), 
           y = max(SWOT_FODAR_df$SWOT_elev_10percentile_m, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nRMSE = ", signif(rmse, 3),
                          "\nn = ", nrow(SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# create an expanded df for swot vs fodar over dist_out_fix
plot_df <- SWOT_FODAR_df %>%
  dplyr::select(dist_out_fix, SWOT_elev_10percentile_m, FODAR_elev_10percentile_m) %>%
  pivot_longer(cols = c(SWOT_elev_10percentile_m, FODAR_elev_10percentile_m),
               names_to = "source", values_to = "elevation_m") %>%
  mutate(source = recode(source,
                         "SWOT_elev_10percentile_m" = "SWOT",
                         "FODAR_elev_10percentile_m" = "FODAR"))

# swot vs fodar over dist_out_fix
ggplot(plot_df) +
  geom_point(aes(x = dist_out_fix / 1000, y = elevation_m, color = source), size = 4) +
  scale_color_manual(values = c("SWOT" = "#00429D", "FODAR" = "#9EBCD8")) +
  xlab("Distance to outlet (km)") +
  ylab("Height (m)") +
  theme_minimal(base_size = 30) +
  labs(color = "Data type")


# CDF plot
ggplot(SWOT_FODAR_df, aes(x = abs(SWOT_elev_10percentile_m - FODAR_elev_10percentile_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Height - FODAR Height (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Height") +
  annotate("text", x = 6, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error, 4)), color = "#222222", size = 6) +
  annotate("text", x = 6, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 
#+ xlim(0, 1.5)

# ---------------------------------------------------------------------------------------------------------------------------
# remove bias from FODAR data

bias <- median(SWOT_FODAR_df$residuals, na.rm = TRUE)
SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m = SWOT_FODAR_df$FODAR_elev_10percentile_m - bias

# 68th & 50th percentile error: wse diff calculation

# Calculate the wse diff FODAR - SWOT (residuals)
SWOT_FODAR_df$residuals_nobias = SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m - SWOT_FODAR_df$SWOT_elev_10percentile_m

# Calculate the 68th percentile error
percentile_68_error_nobias <- quantile(abs(SWOT_FODAR_df$residuals_nobias), 0.68, na.rm=TRUE)
percentile_50_error_nobias <- quantile(abs(SWOT_FODAR_df$residuals_nobias), 0.50, na.rm=TRUE)

#print the result
print(paste("68th Percentile Error Without Bias:", percentile_68_error_nobias))
print(paste("50th Percentile Error Without Bias:", percentile_50_error_nobias))

#csv subset
save_to_csv <- SWOT_FODAR_df %>%
  dplyr::select(node_id, reach_id, residuals, residuals_nobias, FODAR_elev_10percentile_nobias_m, FODAR_elev_10percentile_m, SWOT_elev_10percentile_m, FODAR_FODARFileDate, SWOT_SWOTFileDate, FODAR_elev_mean_m, SWOT_elev_mean_m, FODAR_elev_mean_nobias_m, SWOT_elev_std_m, FODAR_elev_std_m, dist_out_fix)

# save joined_wse_subset to csv
# write.csv(save_to_csv, file = '/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT_FODAR_df_20250322_10ile.csv', row.names = FALSE)

#RMSE
rmse_nobias <- sqrt(mean((SWOT_FODAR_df$FODAR_elev_10percentile_nobias_m - SWOT_FODAR_df$SWOT_elev_10percentile_m)^2))
#RMSE >= MAE, MAE is similar to 50th quantile error

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
  dplyr::select(dist_out_fix, SWOT_elev_10percentile_m, FODAR_elev_10percentile_nobias_m) %>%
  pivot_longer(cols = c(SWOT_elev_10percentile_m, FODAR_elev_10percentile_nobias_m),
               names_to = "source", values_to = "elevation_m") %>%
  mutate(source = recode(source,
                         "SWOT_elev_10percentile_m" = "SWOT",
                         "FODAR_elev_10percentile_nobias_m" = "FODAR"))

# Plot with legend
ggplot(plot_df) +
  geom_point(aes(x = dist_out_fix / 1000, y = elevation_m, color = source), size = 4) +
  scale_color_manual(values = c("SWOT" = "#00429D", "FODAR" = "#9EBCD8")) +
  xlab("Distance to outlet (km)") +
  ylab("Elevation (m)") +
  theme_minimal(base_size = 30) +
  labs(color = "Data type")

# dist to outlet
ggplot(SWOT_FODAR_df) +
  geom_point(aes(x = dist_out_fix / 1000, y = residuals_nobias), size = 4, color = "#00429D") +
  xlab("Distance to outlet (km)") +
  ylab("residuals (m)") +
  theme_minimal(base_size = 30) 

# CDF plot
ggplot(SWOT_FODAR_df, aes(x = abs(SWOT_elev_10percentile_m - FODAR_elev_10percentile_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Height - FODAR Height (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Height") +
  annotate("text", x = 6, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)), color = "#222222", size = 6) +
  annotate("text", x = 6, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 





# ---------------------------------------------------------------------------------------------------------------------------
# All days stats & data viz

# Set working directory to the folder with SWOT_FODAR_dfs
wd <- "/Users/camryn/Documents/UNC/Ice_caval/Tanana"
setwd(wd)

# List of files to join
# either working with means or 10th%ile (change pattern accordingly)
csv_files <- list.files(wd, pattern = "\\ile.csv$", full.names = TRUE)

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
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nn = ", nrow(combined_SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# Calculate the 68th & 50th percentile error
percentile_68_error_nobias <- quantile(abs(combined_SWOT_FODAR_df$residuals_nobias), 0.68, na.rm=TRUE)
percentile_50_error_nobias <- quantile(abs(combined_SWOT_FODAR_df$residuals_nobias), 0.50, na.rm=TRUE)

# CDF plot
ggplot(combined_SWOT_FODAR_df, aes(x = abs(SWOT_elev_mean_m - FODAR_elev_mean_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Elev - FODAR Elev (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Elevation") +
  annotate("text", x = 4, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)), color = "#222222", size = 6) +
  annotate("text", x = 4, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 
