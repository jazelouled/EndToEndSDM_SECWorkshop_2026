# ============================================================
# SCRIPT NAME:
# 03_L1_spacetime_split_Balaenoptera_artificialis.R
#
# PURPOSE:
# This script splits already filtered L1 tracks into segments
# using only:
# - time gaps
# - spatial gaps
#
# IMPORTANT:
# We do NOT use a speed filter here, because speed filtering
# was already done in the previous Douglas step.
#
# WHAT THIS SCRIPT DOES:
# 1. Reads all L1 filtered tracking files
# 2. Computes time gaps between consecutive positions
# 3. Computes spatial gaps between consecutive positions
# 4. Marks positions where a new segment should start
# 5. Assigns a segment ID to each part of the track
# 6. Saves:
#    - one pooled table of all steps
#    - one output file per individual
#
# WHY THIS STEP IS USEFUL:
# Even after filtering, a track may contain very large temporal
# or spatial gaps. These gaps often indicate that the track
# should be split into separate segments rather than treated
# as one continuous movement path.
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
  library(geosphere)
})

message("Starting L1 space-time split workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# Input:
# - all L1 filtered files produced by the Douglas step
#
# Output:
# - one pooled step table
# - one file per individual containing split information
# ============================================================

indir <- here("00inputOutput", "00input", "01processedData", "01tracking", "02L1_douglas", "L1_filtered")
outdir <- here("00inputOutput", "00input", "01processedData", "01tracking", "03L1_spaceTimeSplit")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Safety check
if (!dir.exists(indir)) {
  stop("Input directory not found: ", indir)
}

# ============================================================
# 3. DEFINE SPLITTING THRESHOLDS
# ============================================================
# A new segment starts if:
# - the time gap from the previous point is too large
# - OR the spatial gap from the previous point is too large
#
# Units:
# - dt_cut_hours is in hours
# - dist_cut_km is in kilometers
# ============================================================

dt_cut_hours <- 300
dist_cut_km  <- 600

message("Using thresholds:")
message("  - Time gap threshold     = ", dt_cut_hours, " hours")
message("  - Distance gap threshold = ", dist_cut_km, " km")


# ============================================================
# 4. READ ALL L1 FILES
# ============================================================
# We read one file per individual and store them in a list.
# ============================================================

files_L1 <- list.files(
  indir,
  full.names = TRUE,
  pattern = "^Balaenoptera_artificialis_L1_PTT_.*\\.csv$"
)

if (length(files_L1) == 0) {
  stop("No L1 files found in: ", indir)
}

message("Reading L1 files...")

L1_list <- vector("list", length(files_L1))
names(L1_list) <- basename(files_L1)

for (i in seq_along(files_L1)) {
  
  message("  Reading file ", i, " of ", length(files_L1), ": ", basename(files_L1[i]))
  
  L1_list[[i]] <- read_csv(
    files_L1[i],
    show_col_types = FALSE,
    col_types = cols(
      PTT_ID = col_character(),
      DateTime = col_character(),
      Latitude = col_double(),
      Longitude = col_double(),
      LocationClass = col_character(),
      row_id = col_double(),
      douglas_flag = col_character(),
      douglas_keep = col_logical(),
      final_reason = col_character(),
      mask_value = col_double(),
      is_ocean = col_logical()
    )
  )
}

message("Loaded ", length(L1_list), " L1 files.")


# ============================================================
# 5. PREPARE EMPTY OBJECTS TO STORE RESULTS
# ============================================================
# We will store:
# - one split track table per individual
# - one step table per individual
#
# At the end, all step tables will be merged together.
# ============================================================

tracks_with_segments <- vector("list", length(L1_list))
names(tracks_with_segments) <- names(L1_list)

steps_list <- vector("list", length(L1_list))
names(steps_list) <- names(L1_list)


# ============================================================
# 6. LOOP THROUGH ALL INDIVIDUALS
# ============================================================
# For each individual:
# - clean and sort the track
# - compute temporal and spatial gaps
# - detect where a new segment starts
# - assign segment IDs
# - store the results
# ============================================================

message("Applying space-time split to all tracks...")

for (i in seq_along(L1_list)) {
  
  # ----------------------------------------------------------
  # 6.1 Extract one individual's data and name
  # ----------------------------------------------------------
  
  fname <- names(L1_list)[i]
  df <- L1_list[[i]]
  
  tag_id <- gsub("^Balaenoptera_artificialis_L1_|\\.csv$", "", fname)
  
  message("  Processing ", tag_id, " ...")
  
  # ----------------------------------------------------------
  # 6.2 Clean and sort the data
  # ----------------------------------------------------------
  # We:
  # - remove duplicated positions with same time and coordinates
  # - convert DateTime into a real date-time object
  # - remove rows with missing values
  # - sort positions chronologically
  # ----------------------------------------------------------
  
  df <- df %>%
    distinct(DateTime, Latitude, Longitude, .keep_all = TRUE) %>%
    mutate(
      tag = tag_id,
      date = ymd_hms(DateTime, tz = "UTC")
    ) %>%
    filter(
      !is.na(date),
      !is.na(Longitude),
      !is.na(Latitude)
    ) %>%
    arrange(date)
  
  n <- nrow(df)
  
  message("    Number of valid positions: ", n)
  
  # ----------------------------------------------------------
  # 6.3 Case 1: no valid rows
  # ----------------------------------------------------------
  
  if (n == 0) {
    
    tracks_with_segments[[i]] <- tibble()
    steps_list[[i]] <- tibble()
    
    message("    No valid positions. Skipping.")
    next
  }
  
  # ----------------------------------------------------------
  # 6.4 Case 2: only one position
  # ----------------------------------------------------------
  # If there is only one position, we cannot compute steps.
  # We still keep the point and mark it as the start of a segment.
  # ----------------------------------------------------------
  
  if (n == 1) {
    
    df <- df %>%
      mutate(
        dt_hours = NA_real_,
        dist_km = NA_real_,
        split_flag = TRUE,
        split_reason = "START",
        segment_id = 0
      )
    
    tracks_with_segments[[i]] <- df
    steps_list[[i]] <- tibble()
    
    message("    Only one position. No step table created.")
    next
  }
  
  # ----------------------------------------------------------
  # 6.5 Compute temporal gaps between consecutive positions
  # ----------------------------------------------------------
  # The first row has no previous point, so dt_hours is NA there.
  # ----------------------------------------------------------
  
  dt_hours <- c(
    NA_real_,
    as.numeric(difftime(df$date[-1], df$date[-n], units = "hours"))
  )
  
  # ----------------------------------------------------------
  # 6.6 Compute spatial gaps between consecutive positions
  # ----------------------------------------------------------
  # Again, the first row has no previous point, so dist_km is NA.
  # ----------------------------------------------------------
  
  dist_km <- c(
    NA_real_,
    geosphere::distHaversine(
      cbind(df$Longitude[-n], df$Latitude[-n]),
      cbind(df$Longitude[-1], df$Latitude[-1])
    ) / 1000
  )
  
  # ----------------------------------------------------------
  # 6.7 Identify where the track should be split
  # ----------------------------------------------------------
  # A split happens if:
  # - time gap > threshold
  # - or distance gap > threshold
  #
  # The first row is always the start of a new segment.
  # ----------------------------------------------------------
  
  flag_time <- dt_hours > dt_cut_hours
  flag_dist <- dist_km > dist_cut_km
  
  split_flag <- flag_time | flag_dist
  split_flag[1] <- TRUE
  
  # ----------------------------------------------------------
  # 6.8 Assign the reason for the split
  # ----------------------------------------------------------
  
  split_reason <- rep("OK", n)
  
  split_reason[flag_time & !flag_dist] <- "TIME_GAP"
  split_reason[flag_dist & !flag_time] <- "DISTANCE"
  split_reason[flag_time & flag_dist]  <- "MULTI"
  split_reason[1] <- "START"
  
  # ----------------------------------------------------------
  # 6.9 Assign a segment ID
  # ----------------------------------------------------------
  # Each time split_flag is TRUE, a new segment starts.
  # Example:
  # split_flag = TRUE FALSE FALSE TRUE FALSE
  # segment_id = 0    0     0    1    1
  # ----------------------------------------------------------
  
  segment_id <- cumsum(split_flag) - 1
  
  # ----------------------------------------------------------
  # 6.10 Add these new variables to the full track table
  # ----------------------------------------------------------
  
  df <- df %>%
    mutate(
      dt_hours = dt_hours,
      dist_km = dist_km,
      split_flag = split_flag,
      split_reason = split_reason,
      segment_id = segment_id
    )
  
  # Save full track with segment information
  tracks_with_segments[[i]] <- df
  
  # ----------------------------------------------------------
  # 6.11 Build a step table for this individual
  # ----------------------------------------------------------
  # The step table excludes the first row because it has no
  # previous point, so dt_hours and dist_km are NA there.
  # ----------------------------------------------------------
  
  steps_df <- df %>%
    select(
      tag,
      PTT_ID,
      date,
      Longitude,
      Latitude,
      LocationClass,
      dt_hours,
      dist_km,
      split_flag,
      split_reason,
      segment_id
    ) %>%
    filter(!is.na(dt_hours))
  
  steps_list[[i]] <- steps_df
  
  message("    Number of steps: ", nrow(steps_df))
  message("    Number of segments: ", length(unique(df$segment_id)))
}


# ============================================================
# 7. MERGE ALL STEP TABLES INTO ONE POOLED TABLE
# ============================================================
# This table contains step-level information for all
# individuals together.
# ============================================================

steps_all <- bind_rows(steps_list)

write_csv(
  steps_all,
  file.path(outdir, "Balaenoptera_artificialis_L1_all_steps_spaceTime.csv")
)

message("Global pooled step table saved.")


# ============================================================
# 8. SAVE ONE OUTPUT FILE PER INDIVIDUAL
# ============================================================
# Each file contains the original L1 track plus:
# - dt_hours
# - dist_km
# - split_flag
# - split_reason
# - segment_id
# ============================================================

message("Saving one split file per individual...")

for (i in seq_along(tracks_with_segments)) {
  
  fname <- names(tracks_with_segments)[i]
  tag_id <- gsub("^Balaenoptera_artificialis_L1_|\\.csv$", "", fname)
  
  out_file <- file.path(
    outdir,
    paste0("Balaenoptera_artificialis_L1_spaceTimeSplit_", tag_id, ".csv")
  )
  
  write_csv(
    tracks_with_segments[[i]],
    out_file
  )
  
  message("  Saved split file for ", tag_id)
}


# ============================================================
# 9. FINAL MESSAGE
# ============================================================

message("====================================")
message("Space-time split completed.")
message("Tracks were split using time and distance gaps.")
message("Time gaps were computed in HOURS.")
message("Thresholds used:")
message("  - dt_cut_hours = ", dt_cut_hours)
message("  - dist_cut_km  = ", dist_cut_km)
message("Saved:")
message("  - one split file per tag")
message("  - one pooled steps table")
message("====================================")