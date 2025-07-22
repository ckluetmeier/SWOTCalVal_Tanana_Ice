library(dplyr)
library(tidyverse)
library(lubridate)
library(sf)
library(ncdf4)

# ---------------------------------------------------------------------------------------------------------------------------
# Clip PIXC to Area of Interest (AOI) & concatenate PIXC tiles
# ---------------------------------------------------------------------------------------------------------------------------


# Read AOI shapefile
# ---------------------------------------------------------------------------------------------------------------------------
filename <- '/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWORD_clipped_TN_domain/TN_AOI.shp'
#"/Users/camryn/Documents/UNC/_Tier1_sites/expanded_Yukon_Flats/GIS/YR_AOI.shp"
AOI <- st_read(filename)
AOI_coords <- st_coordinates(AOI)[, 1:2]  # Extract x, y coordinates
AOI_sf <- st_polygon(list(AOI_coords)) %>%  # Convert to polygon
  st_sfc(crs = st_crs(32606)) %>% # crs=WGS84 4326, ITRF14 7912, UTM6N
  st_as_sf() # Convert to sf
AOI_sf = st_transform(AOI_sf, crs=32606) # confirm crs=WGS84

# Load PIXC csv
# ---------------------------------------------------------------------------------------------------------------------------
# Define the folder path
folder_path = '/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixc/UTM6N_tidecorrections'
#'/Users/camryn/Documents/UNC/_Tier1_sites/expanded_Yukon_Flats/SWOT/pixc/lower_YR' # v2.0
all_files = list.files(folder_path, pattern="*.csv", full.names=TRUE) 

# Extract unique identifiers from filenames: start datetime of overpass
unique_ids <- unique(substring(basename(all_files), 30, 40)) #characters 30-44 in the filename string

# Precompile the pattern for extracting short names (characters 30-44)
pattern <- "(.{29})(.{15}).*"

# Loop over unique IDs (datetimes) to merge PIXC tiles based on date & clip to AOI
# ---------------------------------------------------------------------------------------------------------------------------
# create an empty list to store clipped PIXC
PIXCL_list = list()
for (id in unique_ids) {
  
  # Identify files with the same unique ID (start datetime of overpass)
  pair_files = all_files[grepl(id, substring(basename(all_files), 30, 44))]
  
  if (length(pair_files) > 0) {
    # Create a short name for the sf object using characters 30-44
    short_name <- substring(basename(pair_files[1]), 30, 44)
    
    # Read and concatenate the grouped files
    concatenated_df = bind_rows(lapply(pair_files, read_csv))
    
    # Convert the concatenated dataframe to sf object
    concatenated_sf = st_as_sf(concatenated_df,
                               coords = c("longitude", "latitude"),
                               crs = 32606, # WGS84 4326, ITRF14 7912, UTM6N 32606
                               remove = FALSE)
    
    # Use st_intersects to get indices of points within the AOI
    intersects_list = st_intersects(concatenated_sf, AOI_sf, sparse=TRUE)
    intersects_vector = lengths(intersects_list) > 0
    filtered_sf = concatenated_sf[intersects_vector, ]
    
    # Store the sf object in the list with short name as its name
    PIXCL_list[[short_name]] = filtered_sf
  }
}

# Plot all of the merged & clipped PIXC datetimes
# ---------------------------------------------------------------------------------------------------------------------------
# Loop through each item in PIXCL_list
for (id in names(PIXCL_list)) {
  
  # Sample every 10,000th point
  PIXCL_subset = PIXCL_list[[id]] %>%
    filter(row_number() %% 500 == 0)
  
  # Create the plot
  p = ggplot() +
    geom_sf(data = AOI_sf, fill = "lightblue", color = "white") +
    geom_point(data = PIXCL_subset, aes(x = longitude, y = latitude)) +
    labs(title = paste("Subset of PIXCL on", id),
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
  # Print the plot
  print(p)
}

# Save output as CSV or shapefiles
# ---------------------------------------------------------------------------------------------------------------------------
# Directory to save the CSV files (modify as needed)
output_dir <- "/Users/camryn/Documents/UNC/Ice_caval/Tanana/SWOT/pixc/UTM6N_tidecorrections"

# Loop over PIXCL_list and save each sf object as a CSV
for (name in names(PIXCL_list)) {
  sf_object <- PIXCL_list[[name]]
  
  # Drop geometry before saving
  df_to_save <- st_drop_geometry(sf_object)
  
  # Define the output filename
  output_file <- file.path(output_dir, paste0(name, ".csv"))
  
  # Write to CSV
  write_csv(df_to_save, output_file)
}


# Loop over PIXCL_list and save each sf object as a Shapefile
for (name in names(PIXCL_list)) {
  sf_object <- PIXCL_list[[name]]
  
  # Define the output filename
  output_file <- file.path(output_dir, paste0(name, ".shp"))
  
  # Write to Shapefile
  st_write(sf_object, output_file, delete_layer = TRUE)
}
