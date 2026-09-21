# ============================================================
# SCRIPT NAME:
# 06_presAbs_grid_balancing_Balaenoptera_artificialis.R
#
# PURPOSE:
# This script builds a gridded and balanced presence–absence
# dataset from the segment-based simulation outputs.
#
# INPUT:
# - files ending in *_sim_L2_locations.csv
# - each file contains:
#     * observed positions      -> occ = 1
#     * simulated positions     -> occ = 0
#
# WORKFLOW:
# 1. Read all simulation files
# 2. Separate presences and absences
# 3. Assign all records to spatial grid cells
# 4. Aggregate records by:
#      id + segment_id + cell + date
# 5. Remove absences that are too close in space and time
#    to any presence
# 6. Combine presences and filtered absences
# 7. Balance the dataset within each id + segment_id stratum
# 8. Export:
#      - balanced CSV
#      - separate presence/absence map
#      - combined map
#
# WHY THIS STEP IS USEFUL:
# Habitat models often need one final table where presences and
# pseudo-absences are represented in a comparable way.
# This script converts track-based data into a gridded dataset
# ready for environmental extraction and modelling.
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(dplyr)
  library(raster)
  library(readr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(patchwork)
  library(lubridate)
})

sf::sf_use_s2(FALSE)

set.seed(123)

message("Starting gridded balanced presence–absence workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# Input:
# - segment-based simulation outputs from the previous step
#
# Output:
# - balanced gridded CSV
# - separate map of presences and absences
# - combined map
# ============================================================

sim_data <- here("00inputOutput", "00input", "01processedData", "01tracking", "05simulations_Behaviour")
outdir <- here("00inputOutput", "00input", "01processedData", "01tracking", "06PresAbs_grid")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(sim_data)) {
  stop("Simulation directory not found: ", sim_data)
}


# ============================================================
# 3. FIND ALL SIMULATION FILES
# ============================================================
# We read all files ending in:
# *_sim_L2_locations.csv
#
# These files contain both:
# - presences (occ = 1)
# - simulated pseudo-absences (occ = 0)
# ============================================================

files <- list.files(
  sim_data,
  pattern = "_sim_L2_locations\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No *_sim_L2_locations.csv files found in: ", sim_data)
}

message("Number of simulation files found: ", length(files))


# ============================================================
# 4. READ ALL FILES
# ============================================================
# We read each file into a list, then merge them into one table.
# ============================================================

df_list <- vector("list", length(files))

for (i in seq_along(files)) {
  
  message("  Reading file ", i, " of ", length(files), ": ", basename(files[i]))
  
  df_list[[i]] <- fread(files[i])
}

df <- rbindlist(df_list, fill = TRUE)

message("Total rows loaded: ", nrow(df))


# ============================================================
# 5. CHECK THAT REQUIRED COLUMNS EXIST
# ============================================================
# The downstream workflow assumes these columns are present.
# ============================================================

need_cols <- c("id", "segment_id", "simid", "date", "lon", "lat", "occ")
miss <- setdiff(need_cols, names(df))

if (length(miss) > 0) {
  stop("The following required columns are missing: ", paste(miss, collapse = ", "))
}


# ============================================================
# 6. BASIC DATE PARSING AND CLEANING
# ============================================================
# Here we:
# - convert 'date' into POSIXct
# - create a pure Date column called date_day
# - remove rows with missing coordinates or dates
# ============================================================

df[, datetime := as.POSIXct(date, tz = "UTC")]
df[, date_day := as.Date(datetime)]

df <- df[
  !is.na(lon) &
    !is.na(lat) &
    !is.na(datetime) &
    !is.na(date_day)
]

if (nrow(df) == 0) {
  stop("No valid rows remaining after basic cleaning.")
}

message("Rows after basic cleaning: ", nrow(df))
message("Unique individuals: ", length(unique(df$id)))


# ============================================================
# 7. SPLIT PRESENCES AND ABSENCES
# ============================================================
# occ = 1 -> observed positions (presences)
# occ = 0 -> simulated positions (pseudo-absences)
# ============================================================

pres <- copy(df[occ == 1])
abs  <- copy(df[occ == 0])

if (nrow(pres) == 0) stop("No presences found.")
if (nrow(abs)  == 0) stop("No absences found.")

message("Number of presence rows: ", nrow(pres))
message("Number of absence rows: ", nrow(abs))


# ============================================================
# 8. BUILD A SPATIAL GRID
# ============================================================
# We create a regular longitude-latitude raster grid.
# Each point will be assigned to a raster cell.
#
# Grid resolution:
# 0.083 degrees
# ============================================================

xmin <- floor(min(c(pres$lon, abs$lon), na.rm = TRUE))
xmax <- ceiling(max(c(pres$lon, abs$lon), na.rm = TRUE))
ymin <- floor(min(c(pres$lat, abs$lat), na.rm = TRUE))
ymax <- ceiling(max(c(pres$lat, abs$lat), na.rm = TRUE))

grid <- raster(
  extent(xmin, xmax, ymin, ymax),
  res = 0.083,
  crs = CRS("+proj=longlat +datum=WGS84")
)

message("Grid created.")
message("  xmin = ", xmin)
message("  xmax = ", xmax)
message("  ymin = ", ymin)
message("  ymax = ", ymax)


# ============================================================
# 9. ASSIGN EACH POINT TO A GRID CELL
# ============================================================
# We convert each lon-lat pair into a cell ID.
# ============================================================

pres[, cell := cellFromXY(grid, cbind(lon, lat))]
abs[,  cell := cellFromXY(grid, cbind(lon, lat))]


# ============================================================
# 10. AGGREGATE PRESENCES
# ============================================================
# We reduce the data so that we keep only one row per:
# - id
# - segment_id
# - cell
# - date
#
# This avoids having multiple points from the same track in the
# same grid cell on the same day.
# ============================================================

cpres <- pres %>%
  group_by(id, segment_id, cell, date_day) %>%
  summarise(.groups = "drop")

setDT(cpres)
setnames(cpres, "date_day", "date")

message("Aggregated presence rows: ", nrow(cpres))


# ============================================================
# 11. BUILD A GLOBAL PRESENCE TABLE FOR THE FILTER
# ============================================================
# For the spatio-temporal filter, we only need:
# - cell
# - date
#
# This table tells us where and when presences occurred.
# ============================================================

cpres_global <- cpres[, .(cell, date)]
setkey(cpres_global, cell, date)


# ============================================================
# 12. AGGREGATE ABSENCES
# ============================================================
# Same aggregation logic as for presences:
# one row per id + segment_id + cell + date
# ============================================================

cabs <- abs %>%
  group_by(id, segment_id, cell, date_day) %>%
  summarise(.groups = "drop")

setDT(cabs)
setnames(cabs, "date_day", "date")

message("Aggregated absence rows before filtering: ", nrow(cabs))


# ============================================================
# 13. SPATIO-TEMPORAL FILTER FOR ABSENCES
# ============================================================
# Goal:
# remove absences that are too close to any presence
#
# Logic:
# for each presence, define a spatio-temporal neighbourhood:
# - focal cell + 8 neighbouring cells
# - dates from -7 to +7 days
#
# Any absence falling inside that neighbourhood is removed.
#
# This makes pseudo-absences more conservative and avoids
# labelling nearby-used space as absence.
# ============================================================

message("Applying spatio-temporal filter to absences...")

expanded <- vector("list", nrow(cpres_global))

for (i in seq_len(nrow(cpres_global))) {
  
  # Get focal cell and its 8 neighbours
  neigh <- adjacent(
    grid,
    cpres_global$cell[i],
    directions = 8,
    include = TRUE
  )
  
  # Build the temporal window around the presence date
  dates <- seq(
    cpres_global$date[i] - 7,
    cpres_global$date[i] + 7,
    by = 1
  )
  
  # Store all combinations of neighbouring cells and dates
  expanded[[i]] <- CJ(cell = neigh, date = dates)
}

# Merge all spatio-temporal neighbourhoods together
pres_expanded <- unique(rbindlist(expanded))

setkey(pres_expanded, cell, date)
setkey(cabs, cell, date)

# Find absences overlapping the expanded presence neighbourhood
idx <- cabs[
  pres_expanded,
  on = .(cell, date),
  which = TRUE
]

# Mark conflicts
if (length(idx) > 0) {
  cabs[idx, conflict := TRUE]
}

# Keep only absences not in conflict
cabs_clean <- cabs[is.na(conflict)]
cabs_clean[, conflict := NULL]

message("Absences before spatio-temporal filter: ", nrow(cabs))
message("Absences after  spatio-temporal filter: ", nrow(cabs_clean))


# ============================================================
# 14. COMBINE PRESENCES AND FILTERED ABSENCES
# ============================================================

cpres[, occ := 1]
cabs_clean[, occ := 0]

comb <- rbindlist(list(cpres, cabs_clean), fill = TRUE)

message("Combined rows after filtering: ", nrow(comb))


# ============================================================
# 15. STRATIFIED BALANCING BY id + segment_id
# ============================================================
# We balance the dataset within each individual segment.
#
# For each stratum:
# - count number of presences
# - count number of absences
# - keep the same number of both
#
# This avoids strong class imbalance in the final dataset.
# ============================================================

balanced_list <- list()

groups <- unique(comb[, .(id, segment_id)])

message("Balancing presences and absences within each id + segment_id...")

for (i in seq_len(nrow(groups))) {
  
  gid  <- groups$id[i]
  gseg <- groups$segment_id[i]
  
  sub <- comb[id == gid & segment_id == gseg]
  
  pres_sub <- sub[occ == 1]
  abs_sub  <- sub[occ == 0]
  
  np <- nrow(pres_sub)
  na <- nrow(abs_sub)
  
  # Skip groups that cannot be balanced
  if (np == 0 | na == 0) {
    message("  Skipping ", gid, " ", gseg, " (no balance possible)")
    next
  }
  
  # Keep the smallest class size
  n_keep <- min(np, na)
  
  pres_s <- pres_sub[sample(.N, n_keep)]
  abs_s  <- abs_sub[sample(.N, n_keep)]
  
  balanced_list[[length(balanced_list) + 1]] <- rbind(pres_s, abs_s)
  
  message(
    "  Balanced ", gid, " ", gseg,
    " | pres: ", np,
    " | abs: ", na,
    " | kept total: ", n_keep * 2
  )
}

if (length(balanced_list) == 0) {
  stop("No balanced data produced.")
}

balanced_df <- rbindlist(balanced_list, fill = TRUE)

message("Final balanced rows: ", nrow(balanced_df))


# ============================================================
# 16. ADD CELL-CENTRE COORDINATES
# ============================================================
# After aggregation, we want each row to represent the centre
# of its grid cell.
# ============================================================

xy <- xyFromCell(grid, balanced_df$cell)

balanced_df[, lon := xy[, 1]]
balanced_df[, lat := xy[, 2]]


# ============================================================
# 17. EXPORT THE BALANCED CSV
# ============================================================

outfile_csv <- file.path(
  outdir,
  "Balaenoptera_artificialis_PresAbs_grid_balanced.csv"
)

fwrite(balanced_df, outfile_csv)

message("Saved balanced CSV: ", outfile_csv)


# ============================================================
# 18. LOAD LAND POLYGONS FOR MAPS
# ============================================================

land <- ne_countries(scale = "medium", returnclass = "sf")


# ============================================================
# 19. PREPARE PLOTTING TABLES
# ============================================================

pres_plot <- balanced_df[occ == 1]
abs_plot  <- balanced_df[occ == 0]


# ============================================================
# 20. BUILD SEPARATE MAPS
# ============================================================
# LEFT  = presences
# RIGHT = absences
# ============================================================

p1 <- ggplot() +
  geom_sf(
    data = land,
    fill = "grey90",
    color = "grey60"
  ) +
  geom_point(
    data = pres_plot,
    aes(lon, lat),
    color = "blue",
    alpha = 0.7,
    size = 0.01
  ) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = "Presences (balanced, gridded)",
    x = NULL,
    y = NULL
  )

p2 <- ggplot() +
  geom_sf(
    data = land,
    fill = "grey90",
    color = "grey60"
  ) +
  geom_point(
    data = abs_plot,
    aes(lon, lat),
    color = "orange",
    alpha = 0.7,
    size = 0.01
  ) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = "Absences (balanced, gridded)",
    x = NULL,
    y = NULL
  )

p_final <- p1 | p2

outfile_png <- file.path(
  outdir,
  "Balaenoptera_artificialis_PresAbs_grid_balanced.png"
)

ggsave(
  outfile_png,
  p_final,
  width = 12,
  height = 6,
  dpi = 300
)

message("Saved separate presence/absence map: ", outfile_png)


# ============================================================
# 21. BUILD COMBINED MAP
# ============================================================
# We plot absences first and presences after, so presences are
# visually on top.
# ============================================================

balanced_df$occ_label <- ifelse(balanced_df$occ == 1, "Presence", "Absence")

# First absences, then presences on top
balanced_df <- balanced_df[order(balanced_df$occ)]

p_combined <- ggplot() +
  geom_sf(
    data = land,
    fill = "grey90",
    color = "grey60"
  ) +
  geom_point(
    data = balanced_df,
    aes(lon, lat, color = occ_label),
    alpha = 0.3,
    size = 0.05
  ) +
  scale_color_manual(
    values = c(
      "Absence" = "orange",
      "Presence" = "blue"
    )
  ) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = expression(italic("Balaenoptera artificialis") ~ "— balanced presences and absences"),
    x = NULL,
    y = NULL,
    color = NULL
  )

out_png_combined <- file.path(
  outdir,
  "Balaenoptera_artificialis_PresAbs_grid_balanced_combinedMap.png"
)

ggsave(
  out_png_combined,
  p_combined,
  width = 8,
  height = 6,
  dpi = 300
)

message("Saved combined map: ", out_png_combined)


# ============================================================
# 22. FINAL MESSAGE
# ============================================================

message("====================================")
message("Balanced gridded presence–absence dataset completed")
message("Saved:")
message("  - balanced CSV")
message("  - separate presence/absence map")
message("  - combined map")
message("====================================")