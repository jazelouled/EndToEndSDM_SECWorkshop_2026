# ============================================================
# SCRIPT NAME:
# 00_oceanMask.R
#
# PURPOSE:
# Create an ocean mask from the cropped bathymetry raster
# bathymetry_wmed.tif
#
# INPUT:
# - 00inputOutput/00input/00rawData/00enviro/00StaticLayers/bathymetry_wmed.tif
#
# OUTPUT:
# - 00inputOutput/00input/00rawData/00enviro/oceanmask.tif
#
# MASK VALUES:
# - 1 = ocean
# - 0 = land
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(here)
})

message("Starting ocean mask creation...")

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

bathy_file <- here(
  "00inputOutput", "00input", "00rawData", "00enviro",
  "00StaticLayers", "bathymetry_wmed.tif"
)

out_dir <- here(
  "00inputOutput", "00input", "00rawData", "00enviro"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

oceanmask_out <- file.path(out_dir, "oceanmask.tif")

if (!file.exists(bathy_file)) {
  stop("Bathymetry raster not found: ", bathy_file)
}

# ------------------------------------------------------------
# 2. READ BATHYMETRY
# ------------------------------------------------------------

bathy <- rast(bathy_file)

if (nlyr(bathy) > 1) {
  bathy <- bathy[[1]]
}

names(bathy) <- "bathymetry"

vals_bathy <- values(bathy)

if (length(vals_bathy) == 0 || all(is.na(vals_bathy))) {
  stop("Bathymetry raster has no valid values")
}

# ------------------------------------------------------------
# 3. CREATE OCEAN MASK
# Ocean = bathymetry < 0
# Land  = bathymetry >= 0 or NA
# ------------------------------------------------------------

oceanmask <- ifel(!is.na(bathy) & bathy < 0, 1, 0)
names(oceanmask) <- "oceanmask"

# ------------------------------------------------------------
# 4. SAVE
# ------------------------------------------------------------

writeRaster(
  oceanmask,
  filename = oceanmask_out,
  overwrite = TRUE
)

message("Ocean mask saved at:")
message("  ", oceanmask_out)

# ------------------------------------------------------------
# 5. FINAL MESSAGE
# ------------------------------------------------------------

message("====================================")
message("Ocean mask created successfully")
message("1 = ocean")
message("0 = land")
message("====================================")