# ============================================================
# SCRIPT NAME:
# 07_extractEnvironmentalData_Balaenoptera_artificialis.R
#
# PURPOSE:
# Extract environmental values for each presence-absence point
# from the daily environmental stacks.
#
# INPUT:
# - balanced presence-absence dataset
# - one environmental stack per day (.grd)
#
# OUTPUT:
# - one CSV including:
#   presence-absence data + extracted environmental variables
#
# WHY THIS STEP IS USEFUL:
# Habitat models need a table where each observation
# (presence or absence) is linked to the environmental
# conditions at the same place and time.
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(here)
})

message("Starting environmental extraction workflow...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================

presabs_file <- here(
  "00inputOutput", "00input", "01processedData", "01tracking",
  "06PresAbs_grid", "Balaenoptera_artificialis_PresAbs_grid_balanced.csv"
)

stack_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "02presentStacks"
)

out_dir <- here(
  "00inputOutput", "00input", "01processedData", "02habitatModel"
)

out_file <- file.path(
  out_dir,
  "Balaenoptera_artificialis_PresAbs_with_env.csv"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(presabs_file)) {
  stop("Presence-absence file not found: ", presabs_file)
}

if (!dir.exists(stack_dir)) {
  stop("Environmental stack directory not found: ", stack_dir)
}


# ============================================================
# 3. LOAD PRESENCE-ABSENCE DATA
# ============================================================

message("Reading presence-absence dataset...")

presAbs <- read_csv(presabs_file, show_col_types = FALSE)

message("Rows loaded: ", nrow(presAbs))
message("Columns loaded: ", ncol(presAbs))

# ------------------------------------------------------------
# 3.1 Standardize date column
# ------------------------------------------------------------

if ("date" %in% names(presAbs)) {
  presAbs$dateTime <- as.POSIXct(presAbs$date, tz = "UTC")
} else if ("datetime" %in% names(presAbs)) {
  presAbs$dateTime <- as.POSIXct(presAbs$datetime, tz = "UTC")
} else if ("dateTime" %in% names(presAbs)) {
  presAbs$dateTime <- as.POSIXct(presAbs$dateTime, tz = "UTC")
} else {
  stop("The dataset must contain 'date', 'datetime', or 'dateTime'.")
}

presAbs$day <- format(presAbs$dateTime, "%Y-%m-%d")

if (!all(c("lon", "lat") %in% names(presAbs))) {
  stop("The dataset must contain 'lon' and 'lat' columns.")
}


# ============================================================
# 4. FIND AVAILABLE DAILY STACKS
# ============================================================

stack_files <- list.files(
  stack_dir,
  pattern = "^present_stack_\\d{4}-\\d{2}-\\d{2}\\.grd$",
  full.names = TRUE
)

if (length(stack_files) == 0) {
  stop("No daily environmental stacks found in: ", stack_dir)
}

message("Number of daily stacks found: ", length(stack_files))


# ============================================================
# 5. CREATE A LOOKUP TABLE FOR STACK FILES
# ============================================================

stack_table <- data.frame(
  file = stack_files,
  stringsAsFactors = FALSE
)

stack_table$file_name <- basename(stack_table$file)
stack_table$day <- gsub("^present_stack_|\\.grd$", "", stack_table$file_name)

message("Environmental stack lookup table ready.")


# ============================================================
# 6. GET UNIQUE DAYS IN TRACKING DATA
# ============================================================

all_days <- sort(unique(na.omit(presAbs$day)))

message("Number of unique dates in presence-absence data: ", length(all_days))


# ============================================================
# 7. EXPECTED STACK VARIABLES
# ============================================================

stack_vars <- c(
  "bathymetry",
  "slope",
  "distance_to_coast",
  "mlotst",
  "zos",
  "thetao",
  "thetao_gradient",
  "so",
  "so_gradient",
  "uo",
  "vo",
  "chl",
  "nppv"
)


# ============================================================
# 8. LOOP THROUGH DATES AND EXTRACT ENVIRONMENTAL DATA
# ============================================================

results_list <- vector("list", length(all_days))

for (i in seq_along(all_days)) {
  
  d <- all_days[i]
  
  message("====================================")
  message("Processing date ", i, " of ", length(all_days), ": ", d)
  
  df_day <- presAbs[presAbs$day == d, ]
  
  # ----------------------------------------------------------
  # 8.1 Find the matching environmental stack
  # ----------------------------------------------------------
  
  stack_path <- stack_table$file[stack_table$day == d]
  
  if (length(stack_path) == 1 && file.exists(stack_path)) {
    
    message("  Stack found: ", basename(stack_path))
    
    # --------------------------------------------------------
    # 8.2 Load the environmental stack with terra
    # --------------------------------------------------------
    
    env_stack <- terra::rast(stack_path)
    
    message("  Number of layers in stack: ", terra::nlyr(env_stack))
    message("  Layer names: ", paste(names(env_stack), collapse = ", "))
    
    # Safety check
    if (!all(stack_vars %in% names(env_stack))) {
      warning("Stack does not contain all expected variables for date: ", d)
      warning("Missing: ", paste(setdiff(stack_vars, names(env_stack)), collapse = ", "))
    }
    
    # --------------------------------------------------------
    # 8.3 Convert points to SpatVector
    # --------------------------------------------------------
    
    coords <- as.data.frame(df_day[, c("lon", "lat")])
    
    pts <- terra::vect(
      coords,
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    
    # --------------------------------------------------------
    # 8.4 Extract environmental values
    # --------------------------------------------------------
    # We use a 15 km buffer and compute the mean value.
    # terra::extract returns an ID column first, which we remove.
    # --------------------------------------------------------
    
    extracted_vals <- terra::extract(
      env_stack,
      pts,
      buffer = 15000,
      fun = mean,
      na.rm = TRUE
    )
    
    extracted_vals <- as.data.frame(extracted_vals)
    
    # remove terra's ID column
    if ("ID" %in% names(extracted_vals)) {
      extracted_vals <- extracted_vals[, setdiff(names(extracted_vals), "ID"), drop = FALSE]
    }
    
    # force same order and names as stack
    extracted_vals <- extracted_vals[, names(env_stack), drop = FALSE]
    
    # --------------------------------------------------------
    # 8.5 Combine extracted values with the observations
    # --------------------------------------------------------
    
    df_day <- bind_cols(df_day, extracted_vals)
    
  } else {
    
    warning("Missing environmental stack for date: ", d)
    
    # --------------------------------------------------------
    # 8.6 If stack is missing, create NA columns
    # --------------------------------------------------------
    
    for (v in stack_vars) {
      df_day[[v]] <- NA
    }
  }
  
  results_list[[i]] <- df_day
}


# ============================================================
# 9. COMBINE ALL DAYS
# ============================================================

results_df <- bind_rows(results_list)

message("Final table rows: ", nrow(results_df))
message("Final table columns: ", ncol(results_df))


# ============================================================
# 10. SAVE FINAL OUTPUT
# ============================================================

write_csv(results_df, out_file)

message("====================================")
message("Environmental extraction completed successfully.")
message("Output saved to:")
message(out_file)
message("====================================")