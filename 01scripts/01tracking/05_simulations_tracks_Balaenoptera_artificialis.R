# ============================================================
# SCRIPT NAME:
# 05_simulations_tracks_Balaenoptera_artificialis.R
#
# PURPOSE:
# This script generates pseudo-absence / availability tracks
# using correlated random walk surrogate simulations.
#
# INPUT:
# - L2 SSM tracks, already regularized and routed
# - one file per segment
#
# WORKFLOW:
# For each segment:
#   1. The observed routed track is treated as PRESENCE data
#   2. Multiple simulated tracks are generated
#   3. Simulations are constrained to remain in the ocean
#   4. Observed + simulated positions are saved together
#   5. A QC plot is produced
#
# IMPORTANT:
# - Observed positions keep the behavioural index g
# - Simulated positions have g = NA
# - Observed positions have occ = 1
# - Simulated positions have occ = 0
#
# WHY THIS STEP IS USEFUL:
# In SDM workflows with telemetry data, simulated tracks are
# often used to represent available but unused space, that is,
# pseudo-absences or availability samples.
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(here)
  library(raster)
  library(availability)
  library(stringr)
  library(data.table)
  library(dplyr)
  library(readr)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggplot2)
  library(lubridate)
})

sf::sf_use_s2(FALSE)

message("Starting pseudo-absence simulation workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# Input:
# - L2 routed SSM outputs from the previous script
#
# Output:
# - one CSV per segment containing:
#     observed positions + simulated positions
# - one QC plot per segment
# ============================================================

indir <- here("00inputOutput", "00input", "01processedData", "01tracking", "04L2_ssm_behaviour")
outdir <- here("00inputOutput", "00input", "01processedData", "01tracking", "05simulations_Behaviour")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(indir)) {
  stop("Input directory not found: ", indir)
}


# ============================================================
# 3. DEFINE SIMULATION SETTINGS
# ============================================================
# sim_n:
# - number of surrogate simulated tracks per segment
#
# min_n_obs:
# - minimum number of observed locations required to simulate
# ============================================================

sim_n <- 50
min_n_obs <- 10

message("Simulation settings:")
message("  - simulations per segment = ", sim_n)
message("  - minimum observed points = ", min_n_obs)


# ============================================================
# 4. READ ALL L2 SEGMENT FILES
# ============================================================
# We only read files ending in "_3h.csv", which correspond to
# the regularized / routed predicted tracks from the previous step.
# ============================================================

loc_files <- list.files(
  indir,
  full.names = TRUE,
  pattern = "_3h\\.csv$"
)

if (length(loc_files) == 0) {
  stop("No L2 files ending in _3h.csv were found in: ", indir)
}

message("Reading L2 files...")

df_list <- vector("list", length(loc_files))

for (i in seq_along(loc_files)) {
  
  f <- loc_files[i]
  bn <- basename(f)
  
  message("  Reading file ", i, " of ", length(loc_files), ": ", bn)
  
  tmp <- read_csv(f, show_col_types = FALSE)
  
  # ----------------------------------------------------------
  # Extract ID and segment name from the file name
  # ----------------------------------------------------------
  # Example expected file name:
  # Balaenoptera_artificialis_L2_PTT_01_seg0_3h.csv
  #
  # We extract:
  # - id         -> PTT_01
  # - segment_id -> seg0
  # ----------------------------------------------------------
  
  id <- sub("^Balaenoptera_artificialis_L2_", "", bn)
  id <- sub("_seg.*", "", id)
  
  seg <- sub(".*_seg", "seg", bn)
  seg <- sub("_3h\\.csv", "", seg)
  
  tmp$id <- id
  tmp$segment_id <- seg
  
  df_list[[i]] <- tmp
}

# Join all segment files together
df <- rbindlist(df_list, fill = TRUE)

# ------------------------------------------------------------
# Check that the required columns exist
# ------------------------------------------------------------

need_cols <- c("id", "segment_id", "date", "lon", "lat", "g")
miss <- setdiff(need_cols, names(df))

if (length(miss) > 0) {
  stop("The following required columns are missing: ", paste(miss, collapse = ", "))
}

# Standardize types
df$date <- as.POSIXct(df$date, tz = "UTC")

# Keep only valid rows
df <- df[!is.na(lon) & !is.na(lat) & !is.na(date)]

# Sort by individual, segment and date
df <- df[order(id, segment_id, date)]

# Create a unique key for each segment
df$segment_key <- paste(df$id, df$segment_id, sep = "_")

segments <- unique(df$segment_key)

message("Number of segments found: ", length(segments))


# ============================================================
# 5. CREATE AN OCEAN MASK
# ============================================================
# We create a simple raster mask covering the study region.
#
# Coding:
# - 0 = ocean
# - 1 = land
#
# Later, simulations are only accepted if they remain in ocean.
#
# Here we build the mask from land polygons using rnaturalearth.
# ============================================================

message("Creating ocean mask...")

# Create an extent slightly larger than the observed data
ext <- extent(
  floor(min(df$lon, na.rm = TRUE)) - 15,
  ceiling(max(df$lon, na.rm = TRUE)) + 15,
  floor(min(df$lat, na.rm = TRUE)) - 15,
  ceiling(max(df$lat, na.rm = TRUE)) + 15
)

# Raster resolution in degrees
res_deg <- 0.083

# Empty raster template
template <- raster(ext, res = res_deg, crs = CRS("+proj=longlat +datum=WGS84"))
values(template) <- 0

# Load land polygons
land <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

land <- st_transform(land, crs(template))

# Rasterize land
land_r <- rasterize(land, template, field = 1, background = 0)

# Final mask:
# 0 = ocean
# 1 = land
oceanmask <- land_r
oceanmask[is.na(oceanmask)] <- 0

message("Ocean mask ready.")


# ============================================================
# 6. LOAD LAND POLYGONS FOR QC PLOTS
# ============================================================
# These are only used for drawing the plots.
# ============================================================

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()


# ============================================================
# 7. LOOP OVER SEGMENTS
# ============================================================
# For each segment:
# - build the observed track object
# - fit the AR surrogate model
# - simulate multiple pseudo-absence tracks
# - combine observed and simulated data
# - save CSV and QC plot
# ============================================================

for (i in seq_along(segments)) {
  
  segkey <- segments[i]
  message("====================================")
  message("Processing segment: ", segkey)
  
  # ----------------------------------------------------------
  # 7.1 Extract one segment and sort it by time
  # ----------------------------------------------------------
  
  d <- df[df$segment_key == segkey, ]
  d <- d[order(d$date), ]
  
  # Safety check: skip very short segments
  if (nrow(d) < min_n_obs) {
    message("  Segment too short (", nrow(d), " points) -> skipping")
    next
  }
  
  # ----------------------------------------------------------
  # 7.2 Create observed PRESENCE data
  # ----------------------------------------------------------
  # These are the real observed positions from the routed SSM.
  #
  # occ = 1 means presence
  # g is retained because behaviour is defined for observed points
  # ----------------------------------------------------------
  
  obs_all <- data.frame(
    id = d$id,
    segment_id = d$segment_id,
    simid = paste0(segkey, "_OBS"),
    date = as.POSIXct(d$date, tz = "UTC"),
    lon = d$lon,
    lat = d$lat,
    g = d$g,
    occ = 1
  )
  
  # ----------------------------------------------------------
  # 7.3 Fit the surrogate AR movement model
  # ----------------------------------------------------------
  # This model captures the temporal autocorrelation structure
  # of the observed movement track.
  # ----------------------------------------------------------
  
  arfit <- surrogateARModel(d[, c("lon", "lat")])
  
  # Prepare an empty list to store simulated tracks
  sim_list <- vector("list", sim_n)
  
  # ----------------------------------------------------------
  # 7.4 Generate simulated tracks
  # ----------------------------------------------------------
  
  for (s in 1:sim_n) {
    
    message("   - Simulation ", s, " / ", sim_n)
    
    # --------------------------------------------------------
    # Build the point-check function INSIDE the loop
    # --------------------------------------------------------
    # This function is required by surrogateAR.
    # It checks whether a simulated point falls in the ocean.
    #
    # If raster value = 1 -> land -> reject
    # Otherwise -> accept
    # --------------------------------------------------------
    
    point_check_fun <- function(tm, pt) {
      v <- raster::extract(oceanmask, matrix(c(pt[1], pt[2]), nrow = 1))
      if (!is.na(v) && v == 1) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    
    # --------------------------------------------------------
    # Run the surrogate simulation
    # --------------------------------------------------------
    
    simu <- try(
      availability::surrogateAR(
        arfit,
        xs = as.matrix(d[, c("lon", "lat")]),
        ts = as.POSIXct(d$date, tz = "UTC"),
        point.check = point_check_fun,
        fixed = c(TRUE, rep(FALSE, nrow(d) - 1)),
        partial = FALSE
      ),
      silent = TRUE
    )
    
    # Skip failed simulations
    if (inherits(simu, "try-error")) next
    if (is.null(simu)) next
    if (is.null(simu$xs) || nrow(simu$xs) == 0) next
    if (is.null(simu$ts) || length(simu$ts) == 0) next
    
    # Convert simulated times to POSIXct
    sim_dates <- try(as.POSIXct(simu$ts, tz = "UTC"), silent = TRUE)
    
    if (inherits(sim_dates, "try-error")) next
    if (all(is.na(sim_dates))) next
    
    # Build one simulated track table
    sim_list[[s]] <- data.frame(
      id = d$id[1],
      segment_id = d$segment_id[1],
      simid = paste0(segkey, "_", sprintf("%03d", s)),
      date = sim_dates,
      lon = simu$xs[, 1],
      lat = simu$xs[, 2],
      g = NA_real_,
      occ = 0
    )
  }
  
  # ----------------------------------------------------------
  # 7.5 Keep only successful simulations
  # ----------------------------------------------------------
  
  keep_sim <- !vapply(sim_list, is.null, logical(1))
  sim_list <- sim_list[keep_sim]
  
  if (length(sim_list) == 0) {
    message("  No valid simulations -> skipping segment")
    next
  }
  
  sim_all <- rbindlist(sim_list, fill = TRUE)
  
  # Standardize dates
  sim_all$date <- as.POSIXct(sim_all$date, tz = "UTC")
  obs_all$date <- as.POSIXct(obs_all$date, tz = "UTC")
  
  if (nrow(sim_all) == 0) {
    message("  No valid simulations after binding -> skipping segment")
    next
  }
  
  # ----------------------------------------------------------
  # 7.6 Combine observed and simulated data
  # ----------------------------------------------------------
  
  final_all <- rbindlist(list(obs_all, sim_all), fill = TRUE)
  
  # ----------------------------------------------------------
  # 7.7 Save CSV
  # ----------------------------------------------------------
  
  out_csv <- file.path(outdir, paste0(segkey, "_sim_L2_locations.csv"))
  write.csv(final_all, out_csv, row.names = FALSE)
  
  message("  Saved CSV: ", out_csv)
  
  # ----------------------------------------------------------
  # 7.8 Prepare data for the QC plot
  # ----------------------------------------------------------
  # We create:
  # - one background map
  # - all simulated tracks in grey
  # - the observed track in red
  # ----------------------------------------------------------
  
  xl <- extendrange(c(sim_all$lon, obs_all$lon), f = 0)
  yl <- extendrange(c(sim_all$lat, obs_all$lat), f = 0)
  
  # ----------------------------------------------------------
  # 7.9 Build the QC plot
  # ----------------------------------------------------------
  
  p <- ggplot() +
    geom_sf(data = world, fill = "grey30", color = NA) +
    
    # Simulated tracks
    geom_path(
      data = sim_all,
      aes(x = lon, y = lat, group = simid),
      linewidth = 0.4,
      alpha = 0.4,
      color = "grey70"
    ) +
    
    # Observed track
    geom_path(
      data = obs_all,
      aes(x = lon, y = lat),
      linewidth = 1,
      color = "red"
    ) +
    
    # First observed point
    geom_point(
      data = obs_all[1, ],
      aes(x = lon, y = lat),
      shape = 21,
      colour = "red4",
      fill = "white",
      size = 2.5
    ) +
    
    coord_sf(
      xlim = xl,
      ylim = yl,
      expand = TRUE
    ) +
    
    theme_bw() +
    labs(
      title = segkey,
      subtitle = "Observed track in red; simulated tracks in grey",
      x = "Longitude",
      y = "Latitude"
    )
  
  # ----------------------------------------------------------
  # 7.10 Save the QC plot
  # ----------------------------------------------------------
  
  out_png <- file.path(outdir, paste0(segkey, "_sim_L2_locations.png"))
  ggsave(
    out_png,
    p,
    width = 30,
    height = 15,
    units = "cm",
    dpi = 300
  )
  
  message("  Saved plot: ", out_png)
}

# ============================================================
# 8. FINAL MESSAGE
# ============================================================

message("====================================")
message("Pseudo-absence simulations completed")
message("For each valid segment, the script saved:")
message("  - one CSV with observed + simulated locations")
message("  - one QC plot")
message("Observed positions: occ = 1")
message("Simulated positions: occ = 0")
message("Behavioural index g is only kept for observed positions")
message("====================================")