# ============================================================
# SCRIPT NAME:
# 06_buildPresentStack.R
#
# PURPOSE:
# Build one environmental stack per day for the present period.
#
# EACH DAILY STACK WILL CONTAIN:
# - static variables resampled to the PHY grid (0.083°)
# - dynamic variables for that day
# - gradients for:
#   - thetao
#   - so
#
# IMPORTANT:
# - The target grid is the PHY CMEMS resolution (0.083°)
# - Static layers are adapted to that grid
# - BGC layers such as chl and nppv are also resampled to that grid
# - Daily stacks are saved as .grd files
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(here)
  library(stringr)
})

message("Starting present-day environmental stack building...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================

static_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "00staticLayers"
)

dynamic_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "01dynamicLayers", "daily"
)

out_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "02presentStacks"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(static_dir)) {
  stop("Static layer directory not found: ", static_dir)
}

if (!dir.exists(dynamic_dir)) {
  stop("Dynamic layer directory not found: ", dynamic_dir)
}


# ============================================================
# 3. READ STATIC LAYERS
# ============================================================

message("Reading static layers...")

bath_file <- file.path(static_dir, "bathymetry_wmed.tif")
if (!file.exists(bath_file)) {
  stop("Bathymetry file not found: ", bath_file)
}
bathymetry <- rast(bath_file)
names(bathymetry) <- "bathymetry"
message("  Loaded bathymetry")

slope_file <- file.path(static_dir, "slope_wmed.tif")
if (!file.exists(slope_file)) {
  stop("Slope file not found: ", slope_file)
}
slope <- rast(slope_file)
names(slope) <- "slope"
message("  Loaded slope")

dist_file_1 <- file.path(static_dir, "distance_to_coast_wmed.tif")
dist_file_2 <- file.path(static_dir, "dist_coast_wmed.tif")

if (file.exists(dist_file_1)) {
  dist_file <- dist_file_1
} else if (file.exists(dist_file_2)) {
  dist_file <- dist_file_2
} else {
  stop("Distance-to-coast file not found in: ", static_dir)
}

distance_to_coast <- rast(dist_file)
names(distance_to_coast) <- "distance_to_coast"
message("  Loaded distance to coast")


# ============================================================
# 4. LIST ALL DAILY DYNAMIC FILES
# ============================================================

message("Listing daily dynamic layers...")

dynamic_files <- list.files(
  dynamic_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

if (length(dynamic_files) == 0) {
  stop("No daily dynamic raster files found in: ", dynamic_dir)
}

message("  Number of dynamic files found: ", length(dynamic_files))


# ============================================================
# 5. BUILD A LOOKUP TABLE FOR DYNAMIC FILES
# ============================================================

dynamic_table <- data.frame(
  file = dynamic_files,
  stringsAsFactors = FALSE
)

dynamic_table$file_name <- basename(dynamic_table$file)
dynamic_table$file_stub <- str_remove(dynamic_table$file_name, "\\.tif$")
dynamic_table$date <- str_extract(dynamic_table$file_stub, "\\d{4}-\\d{2}-\\d{2}$")
dynamic_table$variable <- str_remove(dynamic_table$file_stub, "_\\d{4}-\\d{2}-\\d{2}$")

dynamic_table <- dynamic_table[!is.na(dynamic_table$date), ]

if (nrow(dynamic_table) == 0) {
  stop("No dynamic files matched the expected naming format: variable_YYYY-MM-DD.tif")
}

message("Dynamic file table built successfully.")


# ============================================================
# 6. IDENTIFY ALL UNIQUE DATES
# ============================================================

all_dates <- sort(unique(dynamic_table$date))

message("Number of unique dates found: ", length(all_dates))


# ============================================================
# 7. VARIABLES EXPECTED IN DAILY STACKS
# ============================================================

# We use the real CMEMS variable names directly
expected_vars <- c(
  "mlotst",
  "zos",
  "thetao",
  "so",
  "uo",
  "vo",
  "chl",
  "nppv"
)


# ============================================================
# 8. LOOP THROUGH DATES AND BUILD ONE STACK PER DAY
# ============================================================

for (i in seq_along(all_dates)) {
  
  this_date <- all_dates[i]
  
  message("====================================")
  message("Processing date ", i, " of ", length(all_dates), ": ", this_date)
  
  daily_rows <- dynamic_table[dynamic_table$date == this_date, ]
  
  if (nrow(daily_rows) == 0) {
    message("  No dynamic layers found for this date -> skipping")
    next
  }
  
  message("  Number of dynamic variables for this day: ", nrow(daily_rows))
  
  # ----------------------------------------------------------
  # 8.1 Find the PHY raster to use as target grid
  # ----------------------------------------------------------
  # We choose thetao as the reference variable because it belongs
  # to the PHY product at 0.083° resolution.
  # ----------------------------------------------------------
  
  phy_row <- daily_rows[daily_rows$variable == "thetao", ]
  
  if (nrow(phy_row) == 0) {
    message("  No thetao raster found for this date -> skipping")
    next
  }
  
  target_grid <- rast(phy_row$file[1])
  
  if (nlyr(target_grid) > 1) {
    target_grid <- target_grid[[1]]
  }
  
  message("  Using thetao as target grid")
  message("  Target resolution: ", paste(round(res(target_grid), 5), collapse = " x "))
  message("  Target extent: ", paste(round(ext(target_grid), 3), collapse = ", "))
  
  # ----------------------------------------------------------
  # 8.2 Resample static variables to target PHY grid
  # ----------------------------------------------------------
  # Static variables are adapted to the dynamic target grid,
  # not the other way around.
  # ----------------------------------------------------------
  
  bathymetry_target <- resample(bathymetry, target_grid, method = "bilinear")
  slope_target <- resample(slope, target_grid, method = "bilinear")
  distance_to_coast_target <- resample(distance_to_coast, target_grid, method = "bilinear")
  
  names(bathymetry_target) <- "bathymetry"
  names(slope_target) <- "slope"
  names(distance_to_coast_target) <- "distance_to_coast"
  
  daily_stack <- c(
    bathymetry_target,
    slope_target,
    distance_to_coast_target
  )
  
  # ----------------------------------------------------------
  # 8.3 Add dynamic variables one by one
  # ----------------------------------------------------------
  
  for (v in seq_along(expected_vars)) {
    
    dynamic_var <- expected_vars[v]
    
    message("  ------------------------------------")
    message("  Looking for variable: ", dynamic_var)
    
    var_row <- daily_rows[daily_rows$variable == dynamic_var, ]
    
    if (nrow(var_row) == 0) {
      message("    Variable not found for this date -> skipping")
      next
    }
    
    dynamic_file <- var_row$file[1]
    
    message("    Reading file: ", basename(dynamic_file))
    
    r <- rast(dynamic_file)
    
    if (nlyr(r) > 1) {
      r <- r[[1]]
    }
    
    names(r) <- dynamic_var
    
    # --------------------------------------------------------
    # 8.3.1 Resample dynamic variable to target PHY grid if needed
    # --------------------------------------------------------
    # This keeps all layers in the same grid.
    # BGC variables such as chl and nppv will be upscaled from
    # 0.25° to the PHY 0.083° grid.
    # --------------------------------------------------------
    
    if (!compareGeom(r, target_grid, stopOnError = FALSE)) {
      message("    Geometry differs from target grid -> resampling")
      r <- resample(r, target_grid, method = "bilinear")
    }
    
    daily_stack <- c(daily_stack, r)
    
    # --------------------------------------------------------
    # 8.3.2 Compute thetao gradient
    # --------------------------------------------------------
    
    if (dynamic_var == "thetao") {
      
      message("    Computing thetao gradient")
      
      thetao_gradient <- terrain(
        r,
        v = "slope",
        unit = "radians",
        neighbors = 8
      )
      
      names(thetao_gradient) <- "thetao_gradient"
      daily_stack <- c(daily_stack, thetao_gradient)
    }
    
    # --------------------------------------------------------
    # 8.3.3 Compute so gradient
    # --------------------------------------------------------
    
    if (dynamic_var == "so") {
      
      message("    Computing so gradient")
      
      so_gradient <- terrain(
        r,
        v = "slope",
        unit = "radians",
        neighbors = 8
      )
      
      names(so_gradient) <- "so_gradient"
      daily_stack <- c(daily_stack, so_gradient)
    }
  }
  
  # ----------------------------------------------------------
  # 8.4 Save the daily stack as .grd
  # ----------------------------------------------------------
  
  out_file <- file.path(
    out_dir,
    paste0("present_stack_", this_date, ".grd")
  )
  
  writeRaster(
    daily_stack,
    filename = out_file,
    overwrite = TRUE
  )
  
  message("  Saved stack: ", basename(out_file))
  message("  Variables in stack: ", paste(names(daily_stack), collapse = ", "))
}


# ============================================================
# 9. FINAL MESSAGE
# ============================================================

message("====================================")
message("Present-day environmental stacks created successfully.")
message("All layers were aligned to the PHY grid (0.083°).")
message("Gradients added for thetao and so.")
message("Output directory: ", out_dir)
message("====================================")