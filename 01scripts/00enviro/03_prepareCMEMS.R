# ============================================================
# SCRIPT NAME:
# 04_prepareCMEMS.R
#
# PURPOSE:
# Read CMEMS NetCDF files, detect the real variables stored
# inside each file, and export one daily GeoTIFF per variable.
#
# IMPORTANT:
# - Physical NetCDF files are multi-variable
# - Variable names must be read from inside the NetCDF
# - We do NOT infer variables from the file name
# - We do NOT force CMEMS rasters to the bathymetry resolution
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(here)
})

message("Starting CMEMS daily layer preparation...")

# ============================================================
# PATHS
# ============================================================

raw_cmems_dir <- here(
  "00inputOutput", "00input", "00rawData", "00enviro", "01CMEMS"
)

template_file <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "00staticLayers", "bathymetry_wmed.tif"
)

out_dynamic_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "01dynamicLayers", "daily"
)

dir.create(out_dynamic_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(raw_cmems_dir)) {
  stop("Raw CMEMS directory not found: ", raw_cmems_dir)
}

if (!file.exists(template_file)) {
  stop("Template file not found: ", template_file)
}

# ============================================================
# TEMPLATE
# ============================================================

template <- rast(template_file)

message("Template loaded.")
message("Template extent: ", paste(round(ext(template), 3), collapse = ", "))
message("Template resolution: ", paste(round(res(template), 5), collapse = " x "))

# ============================================================
# FIND NETCDF FILES
# ============================================================

cmems_files <- list.files(
  raw_cmems_dir,
  pattern = "\\.nc$",
  full.names = TRUE
)

if (length(cmems_files) == 0) {
  stop("No NetCDF files found in: ", raw_cmems_dir)
}

message("Number of NetCDF files found: ", length(cmems_files))

# ============================================================
# LOOP THROUGH FILES
# ============================================================

for (i in seq_along(cmems_files)) {
  
  f <- cmems_files[i]
  bn <- basename(f)
  
  message("====================================")
  message("Processing file ", i, " of ", length(cmems_files))
  message("File: ", bn)
  
  # ----------------------------------------------------------
  # Read subdatasets / variables inside the NetCDF
  # ----------------------------------------------------------
  
  s <- try(sds(f), silent = TRUE)
  
  if (inherits(s, "try-error")) {
    message("  Could not read NetCDF subdatasets -> skipping file")
    next
  }
  
  var_names <- names(s)
  
  if (length(var_names) == 0) {
    message("  No variables found inside file -> skipping file")
    next
  }
  
  message("  Variables found: ", paste(var_names, collapse = ", "))
  
  # ----------------------------------------------------------
  # Loop through variables inside this file
  # ----------------------------------------------------------
  
  for (v in seq_along(var_names)) {
    
    var_out <- var_names[v]
    
    message("  ------------------------------------")
    message("  Processing variable: ", var_out)
    
    r <- try(rast(f, subds = var_names[v]), silent = TRUE)
    
    if (inherits(r, "try-error")) {
      message("    Could not read variable ", var_out, " -> skipping")
      next
    }
    
    if (nlyr(r) == 0) {
      message("    No layers found for variable ", var_out, " -> skipping")
      next
    }
    
    # --------------------------------------------------------
    # Fix longitude if dataset is in 0–360
    # --------------------------------------------------------
    
    xr <- ext(r)
    
    if (xmin(xr) >= 0 && xmax(xr) > 180) {
      message("    Rotating longitude from 0–360 to -180–180")
      r <- try(rotate(r), silent = TRUE)
      
      if (inherits(r, "try-error")) {
        message("    rotate() failed -> skipping variable")
        next
      }
    }
    
    message("    Number of layers: ", nlyr(r))
    message("    Raster extent: ", paste(round(ext(r), 3), collapse = ", "))
    
    # --------------------------------------------------------
    # Time dimension
    # --------------------------------------------------------
    
    layer_time <- try(time(r), silent = TRUE)
    
    if (inherits(layer_time, "try-error") || is.null(layer_time)) {
      message("    No time dimension detected. Using fallback layer indices.")
      layer_time <- NULL
    } else {
      message("    Time dimension detected.")
    }
    
    # --------------------------------------------------------
    # Build marine mask on the CMEMS grid
    # We adapt the bathymetry template to the CMEMS raster,
    # not the other way around
    # --------------------------------------------------------
    
    template_on_r <- try(resample(template, r[[1]], method = "near"), silent = TRUE)
    
    if (inherits(template_on_r, "try-error") || is.null(template_on_r)) {
      message("    Could not build marine mask -> skipping variable")
      next
    }
    
    # --------------------------------------------------------
    # Loop through layers
    # --------------------------------------------------------
    
    for (j in 1:nlyr(r)) {
      
      message("      Processing layer ", j, " of ", nlyr(r))
      
      r_day <- r[[j]]
      
      # ------------------------------------------------------
      # Output date
      # ------------------------------------------------------
      
      if (!is.null(layer_time)) {
        this_date <- as.Date(layer_time[j])
        date_label <- format(this_date, "%Y-%m-%d")
      } else {
        date_label <- paste0("layer_", sprintf("%03d", j))
      }
      
      # ------------------------------------------------------
      # Check if raster has values before masking
      # ------------------------------------------------------
      
      vals_before <- try(values(r_day), silent = TRUE)
      
      if (inherits(vals_before, "try-error") || length(vals_before) == 0) {
        message("        Could not read values -> skipping")
        next
      }
      
      if (all(is.na(vals_before))) {
        message("        All values are already NA -> skipping")
        next
      }
      
      # ------------------------------------------------------
      # Mask to marine cells only
      # ------------------------------------------------------
      
      r_day <- try(mask(r_day, template_on_r), silent = TRUE)
      
      if (inherits(r_day, "try-error") || is.null(r_day)) {
        message("        Mask failed -> skipping")
        next
      }
      
      vals_after <- try(values(r_day), silent = TRUE)
      
      if (inherits(vals_after, "try-error") || length(vals_after) == 0) {
        message("        Could not read values after masking -> skipping")
        next
      }
      
      if (all(is.na(vals_after))) {
        message("        All values are NA after masking -> skipping")
        next
      }
      
      names(r_day) <- var_out
      
      out_file <- file.path(
        out_dynamic_dir,
        paste0(var_out, "_", date_label, ".tif")
      )
      
      writeRaster(
        r_day,
        filename = out_file,
        overwrite = TRUE
      )
      
      message("        Saved: ", basename(out_file))
    }
  }
}

message("====================================")
message("CMEMS daily layers prepared successfully.")
message("Output directory: ", out_dynamic_dir)
message("====================================")