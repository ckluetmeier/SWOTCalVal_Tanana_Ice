library(deldir)
library(dplyr)
library(tidyverse)
library(sf)

# Concatenate PIXCVec tiles
# ---------------------------------------------------------------------------------------------------------------------------

# Import

# Directory with all PIXCVec tiles in csv format
folder_path = '/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixcvec/PGD0/'
all_files = list.files(folder_path, pattern="*.csv", full.names=TRUE)

# Extract unique identifiers from file names: start datetime only down to minutes of overpass
unique_ids <- unique(substring(basename(all_files), 33, 44)) #characters 33-47 in the filename string, for PIXCVec PGD0

# ---------------------------------------------------------------------------------------------------------------------------
# Concatenate
# ---------------------------------------------------------------------------------------------------------------------------

# Pre-compile the pattern for extracting short names (characters 33-47)
pattern <- "(.{29})(.{15}).*"

PIXCVec_list = list()
for (id in unique_ids) {
  
  # Identify files with the same unique ID
  pair_files = all_files[grepl(id, substring(basename(all_files), 33, 47))] # PIC0: 33, 47 OR DevPID0: 38, 50
  
  # Create a short name for the sf object
  short_name <- sub(pattern, "\\2", basename(pair_files[1]))
  
  # Read and concatenate the grouped files
  concatenated_df = bind_rows(lapply(pair_files, function(file) {
    read.csv(file)})) %>% 
    mutate(node = format(node, scientific = FALSE, trim = TRUE))
  
  # Filter out rows where the latitude is 0
  filtered_df = concatenated_df %>% filter(latitude != 0)
  
  # Convert the concatenated dataframe to sf object
  filtered_df = st_as_sf(filtered_df,
                         coords = c("longitude", "latitude"),
                         crs = 4326, # doesn't work in a projected UTM (use global WGS84 or ITRF14)
                         remove = FALSE)
  
  # Store the sf object in the list with its short name
  PIXCVec_list[[id]] = filtered_df
}

# ---------------------------------------------------------------------------------------------------------------------------
# Save concatenate outputs as CSV or shapefiles
# ---------------------------------------------------------------------------------------------------------------------------

# Directory to save the files
output_dir <- "/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixcvec/PGD0/merged/"

# Loop over PIXCVec_list and save each sf object as a CSV
for (name in names(PIXCVec_list)) {
  sf_object <- PIXCVec_list[[name]]
  
  # Drop geometry before saving
  df_to_save <- st_drop_geometry(sf_object)
  
  # Define the output filename
  output_file <- file.path(output_dir, paste0(name, ".csv"))
  
  # Write to CSV
  write.csv(df_to_save, output_file)
}

# Loop over PIXCVec_list and save each sf object as a Shapefile
for (name in names(PIXCVec_list)) {
  sf_object <- PIXCVec_list[[name]]
  
  # Define the output filename
  output_file <- file.path(output_dir, paste0(name, ".shp"))
  
  # Write to Shapefile
  st_write(sf_object, output_file, delete_layer = TRUE)
}

# ---------------------------------------------------------------------------------------------------------------------------
# Filter reaches and date for node polygon generation
# ---------------------------------------------------------------------------------------------------------------------------

# Define reaches of interest
SWORD_reach <- data.frame(reach_id = c(81245000011, 81245000021, 81245000031, 81245000041, 81247100021,
                                       81247100031, 81247100041, 81247100051, 81247100061)) # put small chunk of reaches here

# Combine all points from PIXCVec_list into one sf df
PIXCVec_points <- bind_rows(PIXCVec_list, .id = "timestamp")

# Filter PIXCVec_points to reach_id in SWORD_reach
PIXCVec_AOI_points <- semi_join(PIXCVec_points, SWORD_reach, by = c("reach" = "reach_id"))

# Filter to one day (since the orthos cover PT reaches twice)
PIXCVec_AOI_points <- PIXCVec_AOI_points %>%
  filter(timestamp == '20250403T164')

# possible timestamps:
# 20250322T031
# 20250324T182
# 20250403T164

# Plot to double check
ggplot() +
  geom_sf(data = PIXCVec_AOI_points, color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("PIXCVec points in AOI")

# ---------------------------------------------------------------------------------------------------------------------------
# Build node polygons from PIXCVec
# ---------------------------------------------------------------------------------------------------------------------------

# Ensure PIXCVec_AOI_points is projected (Voronoi works best in projected CRS)
PIXCVec_AOI_points <- st_transform(PIXCVec_AOI_points, 3857)  # Web Mercator projection

# Extract coordinates of points
voronoi_points <- st_coordinates(PIXCVec_AOI_points)
voronoi_data <- deldir(voronoi_points[,1], voronoi_points[,2]) # Compute Voronoi tessellation (Thiessen polygons)

# Convert Voronoi tiles to polygons and ensure they are closed
voronoi_sf <- tile.list(voronoi_data) %>%
  # Ensure polygon is closed by appending the first point at the end
  map(~{
    coords <- cbind(.x$x, .x$y)
    if (!identical(coords[1,], coords[nrow(coords),])) {coords <- rbind(coords, coords[1,])}
    st_polygon(list(coords))
  }) %>%
  st_sfc(crs = 3857) %>%
  st_sf(geometry = .)

# Add reach_id by spatially joining with original points
voronoi_sf <- st_join(voronoi_sf, PIXCVec_AOI_points["reach"], join = st_nearest_feature)
voronoi_sf <- st_join(voronoi_sf, PIXCVec_AOI_points["node"], join = st_nearest_feature)
voronoi_sf_reach <- voronoi_sf %>%
  group_by(reach) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
voronoi_sf_node <- voronoi_sf %>%
  group_by(node) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Convert CRS
voronoi_sf_reach <- st_transform(voronoi_sf_reach, st_crs(4326)) 
voronoi_sf_node <- st_transform(voronoi_sf_node, st_crs(4326)) 

# # Check by plotting the Thiessen polygons
# # Reaches plot
# ggplot() +
#   geom_sf(data = voronoi_sf_reach, aes(fill = as.factor(reach)), alpha = 0.5) +
#   geom_sf(data = PIXCVec_AOI_points, color = "black", size = 1) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("Thiessen Polygons for PIXCVec Reach")
# 
# # Nodes plot
# ggplot() +
#   geom_sf(data = voronoi_sf_node, aes(fill = as.factor(node)), alpha = 0.5) +
#   #geom_sf(data = PIXCVec_AOI_points, color = "black", size = 1) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("Thiessen Polygons for PIXCVec Node")


# Save polygons
# ---------------------------------------------------------------------------------------------------------------------------

st_write(voronoi_sf_node, "/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixcvec/PGD0/polygons/PIXCVec_20250403_node.shp")

st_write(voronoi_sf_reach, "/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixcvec/PGD0/polygons/PIXCVec_20250403_reach.shp")
