library(tidyverse)
library(lubridate)
library(dplyr)

# ---------------------------------------------------------------------------------------------------------------------------
# Compare SWOT PIXC & FODAR 5m DEM heights: PIXEL BY PIXEL
# ---------------------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------------
# read in SWOT & FODAR data
pixel_SWOT_FODAR_df <- read_csv('/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixc/UTM6N_tidecorrections/20250403T164914_with_FODAR_height.csv')
# SWOT data has already been filtered to 10-60km xtrk

# filter to valid FODAR matches (non -9999)
pixel_SWOT_FODAR_df <- pixel_SWOT_FODAR_df %>%
  filter(FODAR_height > 0) 

# ---------------------------------------------------------------------------------------------------------------------------
# Summary stats

# Calculate the elev diff FODAR - SWOT (residuals)
pixel_SWOT_FODAR_df$residuals = pixel_SWOT_FODAR_df$FODAR_height - pixel_SWOT_FODAR_df$height_w_tides

# Calculate the 68th & 50th percentile error
percentile_68_error <- quantile(abs(pixel_SWOT_FODAR_df$residuals), 0.68, na.rm=TRUE)
percentile_50_error <- quantile(abs(pixel_SWOT_FODAR_df$residuals), 0.50, na.rm=TRUE)

# print the result
print(paste("68th Percentile Error:", percentile_68_error))
print(paste("50th Percentile Error:", percentile_50_error))

# correlation test
cor_test <- cor.test(pixel_SWOT_FODAR_df$height_w_tides, pixel_SWOT_FODAR_df$FODAR_height)

# Extract r and p-value
r_value <- cor_test$estimate # Pearson correlation coefficient
p_value <- cor_test$p.value # highly statistically significant is P < 0.001

# ---------------------------------------------------------------------------------------------------------------------------
# data viz

# plot SWOT vs FODAR elevation
ggplot(pixel_SWOT_FODAR_df, aes(x = FODAR_height, y = height_w_tides)) +
  geom_point(size = 1, color = "darkblue") +
  xlab("FODAR elevation (m)") +
  ylab("SWOT elevation (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(pixel_SWOT_FODAR_df$FODAR_height, na.rm = TRUE), 
           y = max(pixel_SWOT_FODAR_df$height_w_tides, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nn = ", nrow(pixel_SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# CDF plot
ggplot(pixel_SWOT_FODAR_df, aes(x = abs(height_w_tides - FODAR_height))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Elev - FODAR Elev (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Elevation") +
  annotate("text", x = 5, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error, 4)), color = "#222222", size = 6) +
  annotate("text", x = 5, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 

# ---------------------------------------------------------------------------------------------------------------------------
# remove bias from FODAR data

bias <- median(pixel_SWOT_FODAR_df$residuals, na.rm = TRUE)
pixel_SWOT_FODAR_df$FODAR_elev_mean_nobias_m = pixel_SWOT_FODAR_df$FODAR_height - bias

# Calculate the wse diff FODAR - SWOT (residuals)
pixel_SWOT_FODAR_df$residuals_nobias = pixel_SWOT_FODAR_df$FODAR_elev_mean_nobias_m - pixel_SWOT_FODAR_df$height_w_tides

# Calculate the 68th percentile error
percentile_68_error_nobias <- quantile(abs(pixel_SWOT_FODAR_df$residuals_nobias), 0.68, na.rm=TRUE)
percentile_50_error_nobias <- quantile(abs(pixel_SWOT_FODAR_df$residuals_nobias), 0.50, na.rm=TRUE)

#print the result
# print(paste("68th Percentile Error Without Bias:", percentile_68_error_nobias))
# print(paste("50th Percentile Error Without Bias:", percentile_50_error_nobias))

# correlation test
cor_test_nobias <- cor.test(pixel_SWOT_FODAR_df$height_w_tides, pixel_SWOT_FODAR_df$FODAR_elev_mean_nobias_m)

# Extract r and p-value
r_value_nobias <- cor_test_nobias$estimate # Pearson correlation coefficient
p_value_nobias <- cor_test_nobias$p.value # 

# plot SWOT vs FODAR elevation
ggplot(pixel_SWOT_FODAR_df, aes(x = FODAR_elev_mean_nobias_m, y = height_w_tides)) +
  geom_point(size = 1, color = "darkblue") +
  xlab("FODAR elevation (m)") +
  ylab("SWOT elevation (m)") +
  theme_minimal(base_size = 30) +
  geom_abline(linetype = "dashed", color = "gray") +  # 1:1 line
  annotate("text", x = min(pixel_SWOT_FODAR_df$FODAR_elev_mean_nobias_m, na.rm = TRUE), 
           y = max(pixel_SWOT_FODAR_df$height_w_tides, na.rm = TRUE), 
           label = paste0("r = ", round(r_value, 4), "\np value = ", signif(p_value, 3),
                          "\nn = ", nrow(pixel_SWOT_FODAR_df)),
           hjust = 0, vjust = 1, size = 8)

# CDF plot
ggplot(pixel_SWOT_FODAR_df, aes(x = abs(height_w_tides - FODAR_elev_mean_nobias_m))) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  labs(x = "SWOT Elev - FODAR Elev (m)", y = "Cumulative Probability", title = "CDF of SWOT - FODAR Elevation") +
  annotate("text", x = 10, y = 0.71, label = paste("|68% diff|:", round(percentile_68_error_nobias, 4)), color = "#222222", size = 6) +
  annotate("text", x = 10, y = 0.53, label = paste("|50% diff|:", round(percentile_50_error_nobias, 4)), color = "#222222", size = 6) +
  theme_minimal(base_size = 20) 

# violin plot by SWOT pixel classification
color_palette <- c("#D86A1A", "#F8A31B", "lightblue", "#00429D", "#6D398B", "gray", "gray")

# Count number of pixels per class
class_counts <- pixel_SWOT_FODAR_df %>%
  mutate(class = as.factor(class)) %>%
  group_by(class) %>%
  summarise(n = n()) %>%
  mutate(y_pos = -0.5)  # Position below zero for aesthetics

# violin plot
ggplot(pixel_SWOT_FODAR_df, aes(x = as.factor(class), y = abs(residuals_nobias), fill = as.factor(class))) + 
  geom_violin(alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.2, fill = "white", outlier.size = 3, lwd = 1) +
  scale_fill_manual(values = color_palette, name = "Class") +
  xlab('SWOT Pixel Class') +
  ylab("|SWOT - FODAR Height| (m)") +
  theme_minimal(base_size = 30) +
  theme(legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))) +
  geom_text(data = class_counts, aes(x = class, y = y_pos, label = paste0("n=", n)), inherit.aes = FALSE, size = 6) +
  ylim(-0.5,5)  # Ensure space for counts with -0.5 lim


# CDF by pixel class
ggplot(pixel_SWOT_FODAR_df, 
       aes(x = abs(height - FODAR_elev_mean_nobias_m), color = as.factor(class))) +
  stat_ecdf(geom = "step", size = 1) +
  geom_hline(yintercept = 0.68, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey") +
  scale_color_manual(values = color_palette, name = "Class") +
  labs(
    x = "|SWOT Elev - FODAR Elev| (m)", 
    y = "Cumulative Probability", 
    title = "CDF of |SWOT - FODAR|"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "right",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# calc error by pixel class
percentiles_by_class <- pixel_SWOT_FODAR_df %>%
  mutate(class = as.factor(class)) %>%
  group_by(class) %>%
  summarise(
    percentile_68_error_nobias = quantile(abs(residuals_nobias), 0.68, na.rm = TRUE),
    percentile_50_error_nobias = quantile(abs(residuals_nobias), 0.50, na.rm = TRUE),
    .groups = 'drop')
print(percentiles_by_class)

# plot residuals vs sig0
pixel_class <- pixel_SWOT_FODAR_df %>%
  filter(class %in% c(3, 4, 5))

color_palette2 <- c("lightblue", "#00429D", "#6D398B")

ggplot(pixel_class, aes(x = sig0, y = abs(residuals), color = as.factor(class))) +
  geom_point(size = 1) +
  xlab("sig0") +
  ylab("residuals (m)") +
  theme_minimal(base_size = 30) +
  scale_color_manual(values = color_palette2, name = "class") +
  xlim(0,60)

