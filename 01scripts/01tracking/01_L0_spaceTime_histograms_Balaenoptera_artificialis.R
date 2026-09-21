# ============================================================
# SCRIPT NAME:
# 01_L0_spaceTime_histograms_Balaenoptera_artificialis.R
#
# PURPOSE:
# This script explores the spacing between consecutive tracking
# positions in the L0 data.
#
# Specifically, it calculates:
# - time gaps between consecutive positions (in minutes)
# - spatial gaps between consecutive positions (in km)
#
# It then produces:
# 1. pooled histograms using all individuals together
# 2. one 2-panel histogram per individual
# 3. one complete table of all steps
#
# WHY THIS STEP IS USEFUL:
# This is an important exploratory step before filtering.
# It helps us understand:
# - how regular or irregular the tracking schedule is
# - how far apart consecutive positions are
# - whether there are obvious large gaps in time or space
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
  library(geosphere)
  library(patchwork)
})

message("Starting L0 space-time histogram workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# Input:
# - all standardized L0 files, one per individual
#
# Output:
# - one pooled steps table
# - one pooled histogram figure
# - one histogram figure per individual
# ============================================================

basedir <- here("00inputOutput", "00input", "01processedData", "01tracking")

indir <- file.path(basedir, "00L0_data")
outdir <- file.path(basedir, "01L0_diagnostics")
outdir_ind <- file.path(outdir, "spaceTime_histograms_individuals")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_ind, recursive = TRUE, showWarnings = FALSE)

# Safety check
if (!dir.exists(indir)) {
  stop("Input directory not found: ", indir)
}


# ============================================================
# 3. READ ALL L0 FILES
# ============================================================
# We now read all individual L0 files and merge them into a
# single table.
# ============================================================

files <- list.files(
  indir,
  pattern = "^Balaenoptera_artificialis_L0_PTT_.*\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No L0 files found in: ", indir)
}

message("Reading L0 files...")

l0_list <- vector("list", length(files))

for (i in seq_along(files)) {
  
  l0_list[[i]] <- read_csv(
    files[i],
    show_col_types = FALSE,
    col_types = cols(
      PTT_ID = col_character(),
      DateTime = col_character(),
      Latitude = col_double(),
      Longitude = col_double(),
      LocationClass = col_character()
    )
  ) %>%
    mutate(file_name = basename(files[i]))
}

# Join all individuals into one table
l0_all <- bind_rows(l0_list)

message(
  "Loaded ",
  nrow(l0_all), " positions from ",
  n_distinct(l0_all$PTT_ID), " individuals."
)


# ============================================================
# 4. STANDARDIZE THE TABLE
# ============================================================
# Here we make sure:
# - PTT_ID is character
# - DateTime is a real datetime object
# - LocationClass is character
# - rows are sorted by individual and time
# ============================================================

l0_all <- l0_all %>%
  mutate(
    PTT_ID = as.character(PTT_ID),
    DateTime = ymd_hms(DateTime, tz = "UTC"),
    LocationClass = as.character(LocationClass)
  ) %>%
  filter(
    !is.na(PTT_ID),
    !is.na(DateTime),
    !is.na(Longitude),
    !is.na(Latitude)
  ) %>%
  arrange(PTT_ID, DateTime)


# ============================================================
# 5. BUILD A STEP TABLE
# ============================================================
# A "step" is the movement between one position and the next.
#
# For each individual, we calculate:
# - the next longitude
# - the next latitude
# - the next time
# - the time gap in minutes
# - the spatial gap in km
#
# This will be the main table used in the rest of the script.
# ============================================================

message("Building step table...")

steps_list <- vector("list", length = length(unique(l0_all$PTT_ID)))
ids <- unique(l0_all$PTT_ID)

for (i in seq_along(ids)) {
  
  id_sel <- ids[i]
  
  df_id <- l0_all %>%
    filter(PTT_ID == id_sel) %>%
    arrange(DateTime)
  
  # Add information about the next position
  df_id <- df_id %>%
    mutate(
      lon_next = lead(Longitude),
      lat_next = lead(Latitude),
      time_next = lead(DateTime)
    )
  
  # Keep only rows where a next position exists
  df_id <- df_id %>%
    filter(
      !is.na(lon_next),
      !is.na(lat_next),
      !is.na(time_next)
    )
  
  # Calculate temporal and spatial gaps
  df_id <- df_id %>%
    mutate(
      dt_minutes = as.numeric(difftime(time_next, DateTime, units = "mins")),
      dist_km = geosphere::distHaversine(
        cbind(Longitude, Latitude),
        cbind(lon_next, lat_next)
      ) / 1000
    )
  
  steps_list[[i]] <- df_id
}

# Merge all step tables together
steps <- bind_rows(steps_list)

# Save the complete step table
write_csv(
  steps,
  file.path(outdir, "Balaenoptera_artificialis_L0_all_steps_spaceTime.csv")
)

message("Step table saved.")


# ============================================================
# 6. COMPUTE POOLED 90TH PERCENTILES
# ============================================================
# The p90 value is used as a visual reference in the histograms.
# It marks the value below which 90% of the data fall.
# ============================================================

message("Computing pooled percentiles...")

p90_all_dt <- quantile(steps$dt_minutes, 0.9, na.rm = TRUE)
p90_all_dist <- quantile(steps$dist_km, 0.9, na.rm = TRUE)


# ============================================================
# 7. CREATE POOLED HISTOGRAMS (ALL INDIVIDUALS TOGETHER)
# ============================================================
# We create:
# - one histogram for time gaps
# - one histogram for distance gaps
# and combine them side by side.
# ============================================================

message("Creating pooled histograms for all tags...")

subtitle_all <- paste0(
  "All tags pooled\n",
  "Blue dashed line = p90"
)

p_all_time <- ggplot(steps, aes(x = dt_minutes)) +
  geom_histogram(
    bins = 80,
    fill = "grey70",
    color = "black",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = p90_all_dt,
    color = "blue",
    linewidth = 1,
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "plain"),
    plot.subtitle = element_text(size = 10, lineheight = 1.05),
    plot.margin = margin(8, 12, 8, 8)
  ) +
  labs(
    title = expression(italic("Balaenoptera artificialis") ~ "– Time gaps (all tags pooled)"),
    subtitle = paste0(subtitle_all, " (", round(p90_all_dt, 1), " min)"),
    x = expression(Delta * "t (minutes)"),
    y = "Frequency"
  )

p_all_dist <- ggplot(steps, aes(x = dist_km)) +
  geom_histogram(
    bins = 80,
    fill = "grey70",
    color = "black",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = p90_all_dist,
    color = "blue",
    linewidth = 1,
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "plain"),
    plot.subtitle = element_text(size = 10, lineheight = 1.05),
    plot.margin = margin(8, 12, 8, 8)
  ) +
  labs(
    title = expression(italic("Balaenoptera artificialis") ~ "– Distance gaps (all tags pooled)"),
    subtitle = paste0(subtitle_all, " (", round(p90_all_dist, 1), " km)"),
    x = "Distance (km)",
    y = "Frequency"
  )

p_all <- p_all_time | p_all_dist

ggsave(
  filename = file.path(outdir, "Balaenoptera_artificialis_L0_spaceTime_histograms_allTags.png"),
  plot = p_all,
  width = 12,
  height = 5.5,
  dpi = 300
)


# ============================================================
# 8. CREATE INDIVIDUAL HISTOGRAMS
# ============================================================
# For each individual, we calculate:
# - p90 time gap
# - p90 distance gap
# - start and end dates
# - number of steps
# - number of days covered
#
# Then we save one 2-panel figure per individual.
# ============================================================

message("Creating individual histograms...")

for (i in seq_along(ids)) {
  
  id_sel <- ids[i]
  
  df_id <- steps %>%
    filter(PTT_ID == id_sel)
  
  # Skip individuals with too few steps
  if (nrow(df_id) < 2) {
    next
  }
  
  # Individual percentiles
  p90_id_dt <- quantile(df_id$dt_minutes, 0.9, na.rm = TRUE)
  p90_id_dist <- quantile(df_id$dist_km, 0.9, na.rm = TRUE)
  
  # Time range
  start_date <- min(df_id$DateTime, na.rm = TRUE)
  end_date   <- max(df_id$time_next, na.rm = TRUE)
  
  # Number of steps and number of days
  n_steps <- nrow(df_id)
  total_days <- as.numeric(as.Date(end_date) - as.Date(start_date)) + 1
  
  subtitle_id <- paste0(
    "Start: ", format(start_date, "%Y-%m-%d %H:%M"), "\n",
    "End: ", format(end_date, "%Y-%m-%d %H:%M"), "\n",
    "Steps: ", n_steps,
    " | Days: ", total_days
  )
  
  # Time-gap histogram
  p_id_time <- ggplot(df_id, aes(x = dt_minutes)) +
    geom_histogram(
      bins = 80,
      fill = "grey70",
      color = "black",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = p90_id_dt,
      color = "blue",
      linewidth = 1,
      linetype = "dashed"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "plain"),
      plot.subtitle = element_text(size = 10, lineheight = 1.05),
      plot.margin = margin(8, 12, 8, 8)
    ) +
    labs(
      title = paste("Track", id_sel, "– Time gaps"),
      subtitle = paste0(
        subtitle_id,
        "\nBlue dashed line = p90 (", round(p90_id_dt, 1), " min)"
      ),
      x = expression(Delta * "t (minutes)"),
      y = "Frequency"
    )
  
  # Distance-gap histogram
  p_id_dist <- ggplot(df_id, aes(x = dist_km)) +
    geom_histogram(
      bins = 80,
      fill = "grey70",
      color = "black",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = p90_id_dist,
      color = "blue",
      linewidth = 1,
      linetype = "dashed"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "plain"),
      plot.subtitle = element_text(size = 10, lineheight = 1.05),
      plot.margin = margin(8, 12, 8, 8)
    ) +
    labs(
      title = paste("Track", id_sel, "– Distance gaps"),
      subtitle = paste0(
        subtitle_id,
        "\nBlue dashed line = p90 (", round(p90_id_dist, 1), " km)"
      ),
      x = "Distance (km)",
      y = "Frequency"
    )
  
  # Combine both panels
  p_id <- p_id_time | p_id_dist
  
  # Save figure
  ggsave(
    filename = file.path(
      outdir_ind,
      paste0("Balaenoptera_artificialis_L0_spaceTime_histograms_", id_sel, ".png")
    ),
    plot = p_id,
    width = 13,
    height = 6.2,
    dpi = 300
  )
  
  message("  Saved histogram figure for ", id_sel)
}


# ============================================================
# 9. FINAL MESSAGE
# ============================================================

message("====================================")
message("Balaenoptera artificialis L0 space-time histograms completed")
message("Saved:")
message("- pooled histograms for all tags")
message("- individual histograms")
message("- steps table")
message("====================================")