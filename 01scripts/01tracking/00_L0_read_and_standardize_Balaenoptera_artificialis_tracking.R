# ============================================================
# SCRIPT NAME:
# 00_L0_read_and_standardize_Balaenoptera_artificialis_tracking.R
#
# GENERAL PURPOSE:
# This script prepares the initial tracking dataset (L0 level)
# for the workshop.
#
# In other words, this is the first script students will use
# to inspect and organize the raw tracking data before any
# filtering or modelling steps.
#
# WHAT THIS SCRIPT DOES:
# 1. Reads the raw tracking dataset
# 2. Renames individuals into simple IDs (PTT_01, PTT_02, ...)
# 3. Produces one global map with all tracks together
# 4. Exports one standardized L0 CSV file per individual
# 5. Produces one QC plot per individual
#
# WHY THIS IS USEFUL:
# - It lets us check that the data were read correctly
# - It lets us inspect the raw tracks visually
# - It organizes the tracking data into a simpler structure
# - It creates one file per individual for later scripts
#
# EXPECTED INPUT:
# 00inputOutput/00input/00rawData/01tracking/simulated_tracking_final.csv
#
# OUTPUTS:
# 00inputOutput/00input/01processedData/01tracking/00L0_data/
#   - Balaenoptera_artificialis_AllTags_rawMap.png
#   - Balaenoptera_artificialis_L0_PTT_XX.csv
#   - plots_individuals/Balaenoptera_artificialis_track_PTT_XX.png
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================
# tidyverse       -> data manipulation + ggplot2
# lubridate       -> dates and times
# sf              -> simple spatial objects
# rnaturalearth   -> land polygons for background maps
# rnaturalearthdata -> support data for rnaturalearth
# grid            -> used internally by some plot elements
# here            -> creates paths relative to the project root
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(grid)
  library(here)
})

# We disable s2 because for this kind of simple mapping workflow
# it can sometimes create topology issues when cropping polygons.
sf::sf_use_s2(FALSE)

message("Starting L0 standardization workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# We use 'here()' so that paths are built relative to the
# project folder. This makes the project portable.
#
# INPUT:
# - one CSV file containing all raw tracking positions
#
# OUTPUT:
# - one folder with the standardized L0 files
# - one folder with the QC plots
# ============================================================

path_tracking <- here("00inputOutput", "00input", "00rawData", "01tracking", "simulated_tracking_final.csv")
out_dir <- here("00inputOutput", "00input", "01processedData", "01tracking", "00L0_data")
out_plots_dir <- file.path(out_dir, "plots_individuals")

# Create output folders if they do not exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_plots_dir, recursive = TRUE, showWarnings = FALSE)

# Safety check: stop early if the input file is missing
if (!file.exists(path_tracking)) {
  stop("Tracking file not found: ", path_tracking)
}


# ============================================================
# 3. READ THE RAW TRACKING DATASET
# ============================================================
# At this point we simply read the file as it is.
# Later we will rename and reorganize some columns.
# ============================================================

message("Reading tracking dataset...")

tracking_raw <- read_csv(
  path_tracking,
  show_col_types = FALSE
)

message(
  "Loaded ",
  nrow(tracking_raw), " positions from ",
  n_distinct(tracking_raw$id), " individuals."
)


# ============================================================
# 4. RENAME INDIVIDUAL IDS INTO A SIMPLE PTT FORMAT
# ============================================================
# The original IDs may not be ideal for teaching purposes.
# We therefore create a simple lookup table:
#
# original ID  ->  PTT_01
# original ID  ->  PTT_02
# etc.
#
# This makes later scripts easier to read and explain.
# ============================================================

message("Renaming individual IDs to PTT format...")

# Create a small table linking original IDs to new PTT IDs
id_lookup <- tibble(
  id_original = sort(unique(tracking_raw$id)),
  PTT_ID = paste0(
    "PTT_",
    stringr::str_pad(
      seq_along(sort(unique(tracking_raw$id))),
      width = 2,
      pad = "0"
    )
  )
)

# Join that table back into the raw dataset
tracking_raw <- tracking_raw %>%
  left_join(id_lookup, by = c("id" = "id_original"))

message("Assigned IDs:")
print(id_lookup)


# ============================================================
# 5. PREPARE A CLEAN VERSION OF THE DATA FOR THE GLOBAL MAP
# ============================================================
# Here we create a simplified object that:
# - converts datetime into POSIXct format
# - renames latitude and longitude
# - keeps only valid coordinates and dates
# - restricts the data to the workshop study area
#
# This object is only for plotting all tracks together.
# ============================================================

tracking_map <- tracking_raw %>%
  mutate(
    DateTime = as.POSIXct(datetime, tz = "UTC"),
    Latitude = lat,
    Longitude = lon,
    source = "Tracking"
  ) %>%
  filter(
    !is.na(DateTime),
    !is.na(Latitude),
    !is.na(Longitude)
  ) %>%
  filter(
    Longitude >= -6, Longitude <= 16,
    Latitude  >= 30, Latitude  <= 46
  )


# ============================================================
# 6. BUILD A GLOBAL MAP WITH ALL TRACKS
# ============================================================
# This is the first visual quality-control step.
#
# QUESTIONS THIS MAP HELPS ANSWER:
# - Are the positions in the expected region?
# - Are there obvious coordinate errors?
# - Do tracks roughly look reasonable?
# ============================================================

message("Building global map...")

# Load world land polygons
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

# Crop land polygons to the workshop study area
world_crop <- st_crop(
  world,
  xmin = -6, xmax = 16,
  ymin = 30, ymax = 46
)

# Convert tracking points to an sf object
tracking_sf <- st_as_sf(
  tracking_map,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Build the global map
p_map <- ggplot() +
  geom_sf(data = world_crop, fill = "grey90", color = "grey40") +
  geom_sf(
    data = tracking_sf,
    aes(color = source),
    size = 0.7,
    alpha = 0.6
  ) +
  coord_sf(
    xlim = c(-6, 16),
    ylim = c(30, 46),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = expression(italic("Balaenoptera artificialis") ~ "- all tracks"),
    color = "Dataset",
    x = "Longitude",
    y = "Latitude"
  )

# Save the global map
ggsave(
  filename = file.path(out_dir, "Balaenoptera_artificialis_AllTags_rawMap.png"),
  plot = p_map,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 7. SPLIT THE DATA BY INDIVIDUAL
# ============================================================
# We now create a list where each element contains one
# individual's track.
#
# This makes it easier to:
# - export one file per individual
# - produce one QC map per individual
# ============================================================

tracking_tags <- split(tracking_raw, tracking_raw$PTT_ID)

message("Number of individual datasets created: ", length(tracking_tags))


# ============================================================
# 8. EXPORT ONE STANDARDIZED L0 FILE PER INDIVIDUAL
# ============================================================
# We now loop through all individuals with a FOR LOOP.
#
# For each individual we:
# - standardize the relevant columns
# - keep only valid coordinates and times
# - sort by time
# - export one CSV file
#
# NOTE:
# This is called L0 because it is still raw tracking data,
# just reorganized into a cleaner format.
# ============================================================

message("Creating standardized L0 files per individual...")

for (i in seq_along(tracking_tags)) {
  
  # Extract one individual's data frame
  df <- tracking_tags[[i]]
  
  # Standardize columns
  df_std <- df %>%
    mutate(
      DateTime = as.POSIXct(datetime, tz = "UTC"),
      Latitude = lat,
      Longitude = lon,
      LocationClass = as.character(argos_class)
    ) %>%
    filter(
      !is.na(DateTime),
      !is.na(Latitude),
      !is.na(Longitude)
    ) %>%
    filter(
      Longitude >= -6, Longitude <= 16,
      Latitude  >= 30, Latitude  <= 46
    ) %>%
    arrange(DateTime) %>%
    select(
      PTT_ID,
      DateTime,
      Latitude,
      Longitude,
      LocationClass
    )
  
  # Extract the tag ID
  tag_id <- unique(df_std$PTT_ID)
  
  # Save only if the object makes sense
  if (length(tag_id) == 1 && nrow(df_std) > 0) {
    
    out_file <- file.path(
      out_dir,
      paste0("Balaenoptera_artificialis_L0_", tag_id, ".csv")
    )
    
    write_csv(df_std, out_file)
    
    message("  Saved L0 file for ", tag_id)
  }
}

message("L0 files exported.")


# ============================================================
# 9. PREPARE SOME OBJECTS NEEDED FOR INDIVIDUAL QC PLOTS
# ============================================================
# We define:
# - a fixed color palette for Argos classes
#
# For the bounding box and text placement we will use explicit
# code inside the loop, so students can see everything happening
# line by line.
# ============================================================

quality_map <- c(
  "3" = "#1b9e77",
  "2" = "#66a61e",
  "1" = "#e6ab02",
  "0" = "#d95f02",
  "A" = "#7570b3",
  "B" = "#e7298a",
  "Z" = "#666666"
)


# ============================================================
# 10. CREATE ONE QC PLOT PER INDIVIDUAL
# ============================================================
# We now use another FOR LOOP.
#
# For each individual we:
# - prepare a clean plotting table
# - compute a local bounding box
# - compute simple track summary statistics
# - build the plot
# - save it to disk
#
# THE QC PLOT SHOWS:
# - land background
# - track line colored by time
# - points colored by Argos class
# - start point in red
# - a summary box with basic information
# ============================================================

message("Creating individual QC plots...")

for (i in seq_along(tracking_tags)) {
  
  # ----------------------------------------------------------
  # 10.1 Extract one individual's raw data
  # ----------------------------------------------------------
  
  df <- tracking_tags[[i]]
  
  # ----------------------------------------------------------
  # 10.2 Prepare a clean table for plotting
  # ----------------------------------------------------------
  
  df_plot <- df %>%
    mutate(
      DateTime = as.POSIXct(datetime, tz = "UTC"),
      Latitude = lat,
      Longitude = lon,
      LocationClass = as.character(argos_class)
    ) %>%
    filter(
      !is.na(DateTime),
      !is.na(Latitude),
      !is.na(Longitude)
    ) %>%
    filter(
      Longitude >= -6, Longitude <= 16,
      Latitude  >= 30, Latitude  <= 46
    ) %>%
    arrange(DateTime) %>%
    select(
      PTT_ID,
      DateTime,
      Latitude,
      Longitude,
      LocationClass
    )
  
  # Extract the tag ID
  tag_id <- unique(df_plot$PTT_ID)
  
  # If there are too few points, skip this individual
  if (!(length(tag_id) == 1 && nrow(df_plot) > 1)) {
    next
  }
  
  # ----------------------------------------------------------
  # 10.3 Make Argos class an ordered factor
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    mutate(
      LocationClass = factor(LocationClass, levels = names(quality_map))
    )
  
  # ----------------------------------------------------------
  # 10.4 Compute a local bounding box for this individual
  # ----------------------------------------------------------
  # We create a local map extent so each plot is zoomed
  # around the individual's track.
  # ----------------------------------------------------------
  
  xr <- range(df_plot$Longitude, na.rm = TRUE)
  yr <- range(df_plot$Latitude, na.rm = TRUE)
  
  # If the track is very narrow, widen the extent slightly
  if (diff(xr) < 0.2) xr <- xr + c(-0.2, 0.2)
  if (diff(yr) < 0.2) yr <- yr + c(-0.2, 0.2)
  
  buffer <- 0.5
  
  xlim <- xr + c(-buffer, buffer)
  ylim <- yr + c(-buffer, buffer)
  
  # Keep map extent inside the workshop domain
  xlim[1] <- max(xlim[1], -6)
  xlim[2] <- min(xlim[2], 16)
  ylim[1] <- max(ylim[1], 30)
  ylim[2] <- min(ylim[2], 46)
  
  # ----------------------------------------------------------
  # 10.5 Extract the first point of the track
  # ----------------------------------------------------------
  # This will be shown in red on the map.
  # ----------------------------------------------------------
  
  start_point <- df_plot[1, ]
  
  # ----------------------------------------------------------
  # 10.6 Compute summary statistics for the text box
  # ----------------------------------------------------------
  
  start_date <- min(df_plot$DateTime, na.rm = TRUE)
  end_date   <- max(df_plot$DateTime, na.rm = TRUE)
  
  n_total <- nrow(df_plot)
  total_days <- as.numeric(as.Date(end_date) - as.Date(start_date)) + 1
  
  quality_counts <- df_plot %>%
    count(LocationClass, name = "n") %>%
    arrange(LocationClass)
  
  quality_text <- paste(
    paste0(quality_counts$LocationClass, ": ", quality_counts$n),
    collapse = " | "
  )
  
  track_text <- paste0(
    "Start: ", format(start_date, "%Y-%m-%d %H:%M"), "\n",
    "End: ", format(end_date, "%Y-%m-%d %H:%M"), "\n",
    "Positions: ", n_total, "\n",
    "Days: ", total_days, "\n",
    "Argos classes: ", quality_text
  )
  
  # Position of the text box inside the map
  x_text <- xlim[1] + 0.03 * diff(xlim)
  y_text <- ylim[2] - 0.03 * diff(ylim)
  
  # ----------------------------------------------------------
  # 10.7 Build the QC plot
  # ----------------------------------------------------------
  
  p_ind <- ggplot() +
    
    # Background land
    geom_sf(data = world_crop, fill = "grey90", color = "grey40") +
    
    # Track line colored by time
    geom_path(
      data = df_plot,
      aes(x = Longitude, y = Latitude, color = DateTime),
      linewidth = 0.7,
      alpha = 0.7
    ) +
    
    # All positions colored by Argos class
    geom_point(
      data = df_plot,
      aes(x = Longitude, y = Latitude, fill = LocationClass),
      shape = 21,
      size = 1.3,
      alpha = 0.6,
      color = "black",
      stroke = 0.15
    ) +
    
    # Start point in red
    geom_point(
      data = start_point,
      aes(x = Longitude, y = Latitude),
      color = "red",
      size = 2.5
    ) +
    
    # Text summary
    annotate(
      "label",
      x = x_text,
      y = y_text,
      label = track_text,
      hjust = 0,
      vjust = 1,
      size = 3,
      label.size = 0.2,
      fill = "white",
      alpha = 0.9
    ) +
    
    # Time color scale
    scale_color_datetime(
      name = "Time",
      date_labels = "%Y-%m-%d",
      low = "blue",
      high = "yellow"
    ) +
    
    # Argos class color scale
    scale_fill_manual(
      values = quality_map,
      limits = names(quality_map),
      drop = FALSE,
      na.value = "grey70"
    ) +
    
    # Legend styling
    guides(
      fill = guide_legend(
        override.aes = list(
          shape = 21,
          size = 2.5,
          alpha = 1,
          color = "black"
        )
      ),
      color = guide_colorbar(barheight = unit(3, "cm"))
    ) +
    
    # Plot extent
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    
    # Theme and labels
    theme_bw() +
    theme(
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.4, "cm")
    ) +
    labs(
      title = bquote(italic("Balaenoptera artificialis") ~ "-" ~ .(tag_id)),
      x = "Longitude",
      y = "Latitude",
      fill = "Argos class"
    )
  
  # ----------------------------------------------------------
  # 10.8 Save the plot
  # ----------------------------------------------------------
  
  out_plot_file <- file.path(
    out_plots_dir,
    paste0("Balaenoptera_artificialis_track_", tag_id, ".png")
  )
  
  ggsave(
    filename = out_plot_file,
    plot = p_ind,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  message("  Saved QC plot for ", tag_id)
}

message("Individual QC plots saved.")
message("L0 standardization workflow completed.")