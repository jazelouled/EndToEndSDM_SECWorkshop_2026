# ============================================================
# SCRIPT NAME:
# 50_predictDaily_and_MeanSD_Balaenoptera_artificialis.R
#
# PURPOSE:
# 1. Predict daily habitat suitability from all present stacks
# 2. Save one daily prediction raster per day
# 3. Build mean and SD rasters across all daily predictions
# 4. Save mean and SD plots with ggplot
#
# INPUT:
# - final RF model (.rds)
# - predictors_used.csv
# - daily present stacks (.grd)
#
# OUTPUT:
# - 00inputOutput/01output/01rasters/daily_predictions_present/
# - 00inputOutput/01output/01rasters/summary_present/
# - 00inputOutput/01output/00figures/mean_prediction_present.png
# - 00inputOutput/01output/00figures/sd_prediction_present.png
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(here)
  library(stringr)
  library(ranger)
  library(ggplot2)
  library(dplyr)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
})

message("Starting daily predictions + mean/SD workflow...")

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

model_file <- here(
  "00inputOutput", "01output", "02models", "01fullModel",
  "rf_full_Balaenoptera_artificialis.rds"
)

predictors_file <- here(
  "00inputOutput", "01output", "02models", "01fullModel",
  "predictors_used.csv"
)

stack_dir <- here(
  "00inputOutput", "00input", "01processedData", "00enviro",
  "02presentStacks"
)

pred_dir <- here(
  "00inputOutput", "01output", "01rasters", "daily_predictions_present"
)

summary_dir <- here(
  "00inputOutput", "01output", "01rasters", "summary_present"
)

fig_dir <- here(
  "00inputOutput", "01output", "00figures"
)

dir.create(pred_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(model_file)) {
  stop("Model file not found: ", model_file)
}

if (!file.exists(predictors_file)) {
  stop("Predictors file not found: ", predictors_file)
}

if (!dir.exists(stack_dir)) {
  stop("Stack directory not found: ", stack_dir)
}

# ------------------------------------------------------------
# 2. LOAD MODEL AND PREDICTORS
# ------------------------------------------------------------

rf_model <- readRDS(model_file)
pred_cols <- fread(predictors_file)$predictors

if (length(pred_cols) == 0) {
  stop("No predictors found in predictors_used.csv")
}

message("Predictors used by the model:")
print(pred_cols)

# ------------------------------------------------------------
# 3. FIND STACKS
# ------------------------------------------------------------

stack_files <- list.files(
  stack_dir,
  pattern = "^present_stack_\\d{4}-\\d{2}-\\d{2}\\.grd$",
  full.names = TRUE
)

if (length(stack_files) == 0) {
  stop("No present stacks found in: ", stack_dir)
}

message("Present stacks found: ", length(stack_files))

# ------------------------------------------------------------
# 4. CUSTOM PREDICTION FUNCTION FOR RANGER
# ------------------------------------------------------------

pred_fun <- function(model, data) {
  p <- predict(model, data = data)$predictions
  
  if (is.matrix(p)) {
    if ("pres" %in% colnames(p)) {
      return(p[, "pres", drop = TRUE])
    } else {
      return(p[, ncol(p), drop = TRUE])
    }
  } else {
    return(as.numeric(p))
  }
}

# ------------------------------------------------------------
# 5. DAILY PREDICTIONS
# ------------------------------------------------------------

pred_files <- character(0)

for (i in seq_along(stack_files)) {
  
  stack_file <- stack_files[i]
  stack_name <- basename(stack_file)
  stack_date <- str_extract(stack_name, "\\d{4}-\\d{2}-\\d{2}")
  
  message("====================================")
  message("Predicting day ", i, " of ", length(stack_files), ": ", stack_date)
  
  stk <- rast(stack_file)
  
  missing_vars <- setdiff(pred_cols, names(stk))
  if (length(missing_vars) > 0) {
    warning("Skipping stack because predictors are missing: ",
            paste(missing_vars, collapse = ", "))
    next
  }
  
  stk_sub <- stk[[pred_cols]]
  
  out_file <- file.path(
    pred_dir,
    paste0("prediction_present_", stack_date, ".tif")
  )
  
  terra::predict(
    object = stk_sub,
    model = rf_model,
    fun = pred_fun,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE,
    wopt = list(names = "pred_present")
  )
  
  pred_files <- c(pred_files, out_file)
  message("Saved prediction: ", basename(out_file))
}

if (length(pred_files) == 0) {
  stop("No daily prediction rasters were created.")
}

# ------------------------------------------------------------
# 6. BUILD MEAN AND SD RASTERS
# ------------------------------------------------------------

message("Building mean and SD rasters...")

pred_stack <- rast(pred_files)

mean_rast <- app(pred_stack, mean, na.rm = TRUE)
names(mean_rast) <- "mean_prediction"

sd_rast <- app(pred_stack, sd, na.rm = TRUE)
names(sd_rast) <- "sd_prediction"

mean_file <- file.path(summary_dir, "mean_prediction_present.tif")
sd_file   <- file.path(summary_dir, "sd_prediction_present.tif")

writeRaster(mean_rast, mean_file, overwrite = TRUE)
writeRaster(sd_rast, sd_file, overwrite = TRUE)

message("Saved mean raster: ", basename(mean_file))
message("Saved SD raster: ", basename(sd_file))

# ------------------------------------------------------------
# 7. PREP DATA FOR GGPLOT
# ------------------------------------------------------------

message("Preparing ggplot maps...")

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

bbox_vals <- as.vector(terra::ext(mean_rast))

xmin <- bbox_vals[1]
xmax <- bbox_vals[2]
ymin <- bbox_vals[3]
ymax <- bbox_vals[4]

# TREURE BUFFER DE 5º
xlim_vals <- c(xmin, xmax)
ylim_vals <- c(ymin, ymax)

rast_to_df <- function(r, value_name) {
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df) <- c("x", "y", value_name)
  df
}

mean_df <- rast_to_df(mean_rast, "value")
sd_df   <- rast_to_df(sd_rast, "value")

# ------------------------------------------------------------
# 8. PLOT MEAN MAP
# ------------------------------------------------------------

p_mean <- ggplot() +
  geom_raster(
    data = mean_df,
    aes(x = x, y = y, fill = value)
  ) +
  geom_sf(
    data = world,
    inherit.aes = FALSE,
    fill = "grey90",
    color = "grey35",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = xlim_vals,
    ylim = ylim_vals,
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    na.value = "transparent",
    name = "Suitability"
  ) +
  theme_bw() +
  labs(
    title = "Mean habitat suitability",
    subtitle = "Balaenoptera artificialis - present daily predictions",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(
  filename = file.path(fig_dir, "mean_prediction_present.png"),
  plot = p_mean,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 9. PLOT SD MAP
# ------------------------------------------------------------

p_sd <- ggplot() +
  geom_raster(
    data = sd_df,
    aes(x = x, y = y, fill = value)
  ) +
  geom_sf(
    data = world,
    inherit.aes = FALSE,
    fill = "grey90",
    color = "grey35",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = xlim_vals,
    ylim = ylim_vals,
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    na.value = "transparent",
    name = "SD"
  ) +
  theme_bw() +
  labs(
    title = "Temporal standard deviation of habitat suitability",
    subtitle = "Balaenoptera artificialis - present daily predictions",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(
  filename = file.path(fig_dir, "sd_prediction_present.png"),
  plot = p_sd,
  width = 8,
  height = 6,
  dpi = 300
)

message("====================================")
message("Daily predictions completed.")
message("Daily rasters saved in: ", pred_dir)
message("Mean/SD rasters saved in: ", summary_dir)
message("Figures saved in: ", fig_dir)
message("====================================")