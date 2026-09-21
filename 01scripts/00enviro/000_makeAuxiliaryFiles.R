# ============================================================
# SCRIPT NAME:
# 000_makeAuxiliaryFiles.R
#
# PURPOSE:
# Create the two auxiliary files needed by the CMEMS download:
# 1) tracking_dates.txt
# 2) bbox_env.txt
#
# INPUT:
# - final tracking CSV
#
# OUTPUT:
# - 00inputOutput/00input/00rawData/01tracking/00auxiliaryFiles/tracking_dates.txt
# - 00inputOutput/00input/00rawData/01tracking/00auxiliaryFiles/bbox_env.txt
#
# FORMAT OF bbox_env.txt:
# MIN_LON=...
# MAX_LON=...
# MIN_LAT=...
# MAX_LAT=...
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(here)
})

message("Starting auxiliary file creation...")

# ------------------------------------------------------------
# 1. INPUT FILE
# ------------------------------------------------------------

tracking_file <- here(
  "00inputOutput", "00input", "00rawData", "01tracking",
  "simulated_tracking_final.csv"
)

if (!file.exists(tracking_file)) {
  stop("Tracking file not found: ", tracking_file)
}

# ------------------------------------------------------------
# 2. OUTPUT DIRECTORY
# ------------------------------------------------------------

aux_dir <- here(
  "00inputOutput", "00input", "00rawData", "01tracking",
  "00auxiliaryFiles"
)

dir.create(aux_dir, recursive = TRUE, showWarnings = FALSE)

dates_file <- file.path(aux_dir, "tracking_dates.txt")
bbox_file  <- file.path(aux_dir, "bbox_env.txt")

# ------------------------------------------------------------
# 3. READ TRACKING DATA
# ------------------------------------------------------------

trk <- fread(tracking_file)

# detect date/time column
if ("datetime" %in% names(trk)) {
  trk[, dateTime := as.POSIXct(datetime, tz = "UTC")]
} else if ("dateTime" %in% names(trk)) {
  trk[, dateTime := as.POSIXct(dateTime, tz = "UTC")]
} else if ("date" %in% names(trk)) {
  trk[, dateTime := as.POSIXct(date, tz = "UTC")]
} else {
  stop("No valid time column found. Expected one of: datetime, dateTime, date")
}

# check coordinates
if (!all(c("lon", "lat") %in% names(trk))) {
  stop("Tracking file must contain columns named 'lon' and 'lat'")
}

# keep only valid rows
trk <- trk[
  !is.na(lon) &
    !is.na(lat) &
    !is.na(dateTime)
]

if (nrow(trk) == 0) {
  stop("No valid rows left after cleaning tracking data")
}

message("Valid tracking rows: ", nrow(trk))

# ------------------------------------------------------------
# 4. CREATE tracking_dates.txt
# one date per line: YYYY-MM-DD
# ------------------------------------------------------------

tracking_dates <- sort(unique(as.Date(trk$dateTime)))

writeLines(
  format(tracking_dates, "%Y-%m-%d"),
  dates_file
)

message("tracking_dates.txt created")
message("Number of unique dates: ", length(tracking_dates))

# ------------------------------------------------------------
# 5. CREATE bbox_env.txt
# format required by the .sh script:
# MIN_LON=...
# MAX_LON=...
# MIN_LAT=...
# MAX_LAT=...
# ------------------------------------------------------------

buffer_deg <- 1

min_lon <- floor(min(trk$lon, na.rm = TRUE)) - buffer_deg
max_lon <- ceiling(max(trk$lon, na.rm = TRUE)) + buffer_deg
min_lat <- floor(min(trk$lat, na.rm = TRUE)) - buffer_deg
max_lat <- ceiling(max(trk$lat, na.rm = TRUE)) + buffer_deg

bbox_lines <- c(
  paste0("MIN_LON=", min_lon),
  paste0("MAX_LON=", max_lon),
  paste0("MIN_LAT=", min_lat),
  paste0("MAX_LAT=", max_lat)
)

writeLines(bbox_lines, bbox_file)

message("bbox_env.txt created")
message("LON: ", min_lon, " to ", max_lon)
message("LAT: ", min_lat, " to ", max_lat)

# ------------------------------------------------------------
# 6. FINAL MESSAGE
# ------------------------------------------------------------

message("====================================")
message("Auxiliary files created successfully")
message("Saved files:")
message(" - ", dates_file)
message(" - ", bbox_file)
message("====================================")