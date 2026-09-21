# ============================================================
# SCRIPT NAME:
# 03_prepareStaticLayers.R
#
# PURPOSE:
# Prepare static environmental layers for the workshop:
# - bathymetry
# - slope
# - distance to coast
#
# INPUT:
# - one bathymetry raster in:
#   00inputOutput/00input/00rawData/03StaticLayers/
#
# OUTPUT:
# - processed static rasters in:
#   00inputOutput/00input/01processedData/00enviro/00staticLayers/
#
# NOTES:
# - This script assumes marine bathymetry is negative
# - Distance to coast is calculated for marine cells only
# - Output resolution and extent are inherited from the
#   cropped bathymetry raster
# ============================================================

# ============================================================
# 1. LOAD PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(here)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

sf::sf_use_s2(FALSE)

message("Starting static layer preparation...")

# ============================================================
# 2. PATHS
# ============================================================

raw_static_dir <- here(
  "00inputOutput", "00input", "00rawData", "00enviro", "00StaticLayers"
)

out_static_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro", "00staticLayers"
)

dir.create(out_static_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 3. STUDY AREA
# ============================================================
# Western Mediterranean workshop domain

study_ext <- ext(-6, 16, 30, 46)

# ============================================================
# 4. FIND AND READ BATHYMETRY
# ============================================================

bathy_files <- list.files(raw_static_dir, full.names = TRUE)

if (length(bathy_files) == 0) {
  stop("No bathymetry file found in: ", raw_static_dir)
}

if (length(bathy_files) > 1) {
  message("More than one file found. Using the first one:")
  message(bathy_files[1])
}

bathy <- rast(bathy_files[1])
names(bathy) <- "bathymetry_raw"

message("Bathymetry loaded.")

# ============================================================
# 5. CROP TO STUDY AREA
# ============================================================

bathy <- crop(bathy, study_ext)

# Keep only marine cells
# Assumption: ocean = negative values
bathy <- ifel(!is.na(bathy) & bathy < 0, bathy, NA)

names(bathy) <- "bathymetry"

message("Bathymetry cropped to study area and masked to ocean.")

# ============================================================
# 6. SAVE CROPPED BATHYMETRY
# ============================================================

writeRaster(
  bathy,
  filename = file.path(out_static_dir, "bathymetry_wmed.tif"),
  overwrite = TRUE
)

message("Saved cropped bathymetry.")

# ============================================================
# 7. CREATE SLOPE
# ============================================================
# terrain(..., v = "slope") returns slope from neighbouring cells
# unit = "degrees" gives slope in degrees

slope <- terrain(
  bathy,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)

# Keep only marine cells
slope <- mask(slope, bathy)

names(slope) <- "slope"

writeRaster(
  slope,
  filename = file.path(out_static_dir, "slope_wmed.tif"),
  overwrite = TRUE
)

message("Saved slope raster.")

# ============================================================
# 8. CREATE DISTANCE TO COAST
# ============================================================
# Strategy:
# - create a land polygon layer
# - rasterize land onto the bathymetry grid
# - compute distance from each marine cell to nearest land cell

message("Building land polygons...")

land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_make_valid(land)

# Crop polygons to a slightly larger extent so coastlines near
# the border are still represented correctly
land_crop <- st_crop(
  land,
  xmin = xmin(study_ext) - 1,
  xmax = xmax(study_ext) + 1,
  ymin = ymin(study_ext) - 1,
  ymax = ymax(study_ext) + 1
)

land_vect <- vect(land_crop)

message("Rasterizing land...")

# Rasterize land on the same grid as bathymetry
land_raster <- rasterize(
  land_vect,
  bathy,
  field = 1,
  background = NA
)

# Distance to nearest non-NA cell in land_raster
# Output units are meters if the CRS is projected, but here the
# raster is likely in lon/lat, so we should project first.

message("Projecting rasters for distance-to-coast calculation...")

# Use a projected CRS for proper distance in meters
target_crs <- "EPSG:3035"

bathy_proj <- project(bathy, target_crs)
land_proj  <- project(land_raster, target_crs, method = "near")

message("Computing distance to coast...")

dist_coast_proj <- distance(land_proj)

# Keep only marine cells
dist_coast_proj <- mask(dist_coast_proj, bathy_proj)

# Reproject back to the bathymetry grid
dist_coast <- project(dist_coast_proj, bathy, method = "bilinear")
dist_coast <- mask(dist_coast, bathy)

names(dist_coast) <- "dist_coast"

writeRaster(
  dist_coast,
  filename = file.path(out_static_dir, "distance_to_coast_wmed.tif"),
  overwrite = TRUE
)

message("Saved distance-to-coast raster.")

# ============================================================
# 9. FINAL MESSAGE
# ============================================================

message("====================================")
message("Static layers prepared successfully")
message("Saved:")
message(" - bathymetry_wmed.tif")
message(" - slope_wmed.tif")
message(" - distance_to_coast_wmed.tif")
message("====================================")