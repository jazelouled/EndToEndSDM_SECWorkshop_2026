# ============================================================
# SCRIPT NAME:
# 04_L2_ssm_by_segment_Balaenoptera_artificialis_QC_routePath.R
#
# PURPOSE:
# This script fits a state-space model (SSM) to each space-time
# segment of each individual track.
#
# The model used is:
# - aniMotum
# - model = "mp"
# - continuous behavioural index g
#
# For each segment, the script:
# 1. checks whether the segment is long enough
# 2. fits the SSM
# 3. reroutes the predicted positions with route_path()
# 4. saves the routed predicted track
# 5. saves a convergence summary
# 6. produces a QC figure with two panels:
#    LEFT  = full raw track, with active segment highlighted
#    RIGHT = SSM routed track for that segment only
#
# IMPORTANT:
# This step happens AFTER:
# - Douglas filtering
# - space-time splitting
#
# So here we are no longer working on the raw track, but on
# segments that are assumed to be more coherent.
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
  library(aniMotum)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(patchwork)
})

sf::sf_use_s2(FALSE)

message("Starting L2 SSM workflow for Balaenoptera artificialis...")


# ============================================================
# 2. DEFINE INPUT AND OUTPUT PATHS
# ============================================================
# Input:
# - one space-time split file per individual
#
# Output:
# - one regularized/routed file per segment
# - one convergence file per segment
# - one QC figure per segment
# ============================================================

indir <- here("00inputOutput", "00input", "01processedData", "01tracking", "03L1_spaceTimeSplit")
outdir <- here("00inputOutput", "00input", "01processedData", "01tracking", "04L2_ssm_behaviour")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(indir)) {
  stop("Input directory not found: ", indir)
}


# ============================================================
# 3. DEFINE MODEL PARAMETERS
# ============================================================
# dt_ssm:
# - regularization time step in hours
#
# route_map_scale:
# - scale used internally by route_path
#
# route_buffer_m:
# - buffer used internally by route_path, in meters
#
# min_n_obs:
# - minimum number of observed locations required to try a fit
#
# min_span_hr:
# - minimum segment duration required to try a fit
# ============================================================

dt_ssm <- 3

route_map_scale <- 10
route_buffer_m  <- 20000

min_n_obs   <- 8
min_span_hr <- 12

message("Using SSM settings:")
message("  - time step      = ", dt_ssm, " h")
message("  - map scale      = ", route_map_scale, " km")
message("  - route buffer   = ", route_buffer_m, " m")
message("  - min points     = ", min_n_obs)
message("  - min span       = ", min_span_hr, " h")


# ============================================================
# 4. READ ALL SPACE-TIME SPLIT FILES
# ============================================================
# These are the outputs from the previous script.
# Each file contains one individual track, already split into
# segments using time and distance gaps.
# ============================================================

files_split <- list.files(
  indir,
  full.names = TRUE,
  pattern = "^Balaenoptera_artificialis_L1_spaceTimeSplit_PTT_.*\\.csv$"
)

if (length(files_split) == 0) {
  stop("No space-time split files found in: ", indir)
}

message("Reading split files...")

split_list <- vector("list", length(files_split))
names(split_list) <- basename(files_split)

for (i in seq_along(files_split)) {
  
  message("  Reading file ", i, " of ", length(files_split), ": ", basename(files_split[i]))
  
  split_list[[i]] <- read_csv(
    files_split[i],
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
      is_ocean = col_logical(),
      tag = col_character(),
      date = col_character(),
      dt_hours = col_double(),
      dist_km = col_double(),
      split_flag = col_logical(),
      split_reason = col_character(),
      segment_id = col_double()
    )
  )
}

message("Loaded ", length(split_list), " split files.")


# ============================================================
# 5. LOAD LAND POLYGONS FOR QC PLOTS
# ============================================================
# These polygons are only used for the QC maps.
# ============================================================

message("Loading land polygons for QC plots...")

world <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(4326) %>%
  st_make_valid()


# ============================================================
# 6. LOOP OVER INDIVIDUALS
# ============================================================
# For each individual:
# - clean the data
# - split by segment_id
# - loop over segments
# - fit SSM if the segment is long enough
# ============================================================

for (i in seq_along(split_list)) {
  
  # ----------------------------------------------------------
  # 6.1 Extract one individual's file and assign a tag ID
  # ----------------------------------------------------------
  
  fname <- names(split_list)[i]
  tag_id <- gsub("^Balaenoptera_artificialis_L1_spaceTimeSplit_|\\.csv$", "", fname)
  
  message("====================================")
  message("Processing tag: ", tag_id)
  
  df <- split_list[[i]] %>%
    mutate(
      id   = tag_id,
      date = ymd_hms(DateTime, tz = "UTC"),
      lc   = as.character(LocationClass),
      lon  = Longitude,
      lat  = Latitude
    ) %>%
    arrange(date) %>%
    filter(
      !is.na(lon),
      !is.na(lat),
      !is.na(date)
    )
  
  # If too few rows in the whole track, skip
  if (nrow(df) < min_n_obs) {
    message("  Whole track has too few observations. Skipping individual.")
    next
  }
  
  # ----------------------------------------------------------
  # 6.2 Split this individual's track by segment_id
  # ----------------------------------------------------------
  
  segments <- split(df, df$segment_id)
  
  # ----------------------------------------------------------
  # 6.3 Compute a bounding box for the FULL raw track
  # ----------------------------------------------------------
  # This will be used in the left QC panel.
  # ----------------------------------------------------------
  
  xr_raw <- range(df$lon, na.rm = TRUE)
  yr_raw <- range(df$lat, na.rm = TRUE)
  
  if (diff(xr_raw) < 0.2) xr_raw <- xr_raw + c(-0.2, 0.2)
  if (diff(yr_raw) < 0.2) yr_raw <- yr_raw + c(-0.2, 0.2)
  
  bb_raw_xlim <- c(xr_raw[1] - 1, xr_raw[2] + 1)
  bb_raw_ylim <- c(yr_raw[1] - 1, yr_raw[2] + 1)
  
  # ----------------------------------------------------------
  # 6.4 LOOP OVER SEGMENTS
  # ----------------------------------------------------------
  
  for (j in seq_along(segments)) {
    
    seg_name <- names(segments)[j]
    seg <- segments[[j]] %>%
      arrange(date) %>%
      filter(
        !is.na(lon),
        !is.na(lat),
        !is.na(date)
      )
    
    # --------------------------------------------------------
    # 6.5 Check minimum number of observations
    # --------------------------------------------------------
    
    if (nrow(seg) < min_n_obs) {
      message("  Segment ", seg_name, " has too few points -> skipping")
      next
    }
    
    # --------------------------------------------------------
    # 6.6 Check minimum segment duration
    # --------------------------------------------------------
    
    span_hr <- as.numeric(difftime(max(seg$date), min(seg$date), units = "hours"))
    
    if (is.na(span_hr) || span_hr < min_span_hr) {
      message("  Segment ", seg_name, " too short (", round(span_hr, 1), " h) -> skipping")
      next
    }
    
    seg_tag <- paste0(tag_id, "_seg", seg_name)
    message("  Processing segment: ", seg_tag)
    
    # --------------------------------------------------------
    # 6.7 Build the input table required by aniMotum
    # --------------------------------------------------------
    # aniMotum expects these columns:
    # - id
    # - date
    # - lc
    # - lon
    # - lat
    # --------------------------------------------------------
    
    indata <- seg %>%
      select(id, date, lc, lon, lat)
    
    # --------------------------------------------------------
    # 6.8 Fit the SSM
    # --------------------------------------------------------
    # Model:
    # - "mp" = move persistence model
    # - behavioural index g is continuous
    #
    # We use try() so the script does not stop if one segment fails.
    # --------------------------------------------------------
    
    fit <- try(
      fit_ssm(
        indata,
        model = "mp",
        time.step = dt_ssm,
        map = list(rho_o = factor(NA)),
        control = ssm_control(verbose = 0)
      ),
      silent = TRUE
    )
    
    if (inherits(fit, "try-error")) {
      message("    fit_ssm failed -> skipping segment")
      next
    }
    
    if (is.null(fit$ssm) || length(fit$ssm) == 0) {
      message("    SSM object empty -> skipping segment")
      next
    }
    
    # --------------------------------------------------------
    # 6.9 Route the predicted path
    # --------------------------------------------------------
    # This attempts to reroute predicted positions in a way that
    # respects land barriers.
    # --------------------------------------------------------
    
    fit_routed <- try(
      route_path(
        fit,
        what = "predicted",
        map_scale = route_map_scale,
        buffer = route_buffer_m
      ),
      silent = TRUE
    )
    
    if (inherits(fit_routed, "try-error")) {
      message("    route_path failed -> skipping segment")
      next
    }
    
    # --------------------------------------------------------
    # 6.10 Extract routed predicted positions
    # --------------------------------------------------------
    
    pred <- try(
      grab(fit_routed, what = "rerouted", as_sf = FALSE) %>%
        arrange(date),
      silent = TRUE
    )
    
    if (inherits(pred, "try-error") || is.null(pred) || nrow(pred) == 0) {
      message("    grab(rerouted) failed or empty -> skipping segment")
      next
    }
    
    # --------------------------------------------------------
    # 6.11 Check that behavioural output exists
    # --------------------------------------------------------
    
    if (!("g" %in% names(pred))) {
      message("    behavioural column 'g' not found -> skipping segment")
      next
    }
    
    # --------------------------------------------------------
    # 6.12 Save routed predictions
    # --------------------------------------------------------
    
    out_pred_file <- file.path(
      outdir,
      paste0("Balaenoptera_artificialis_L2_", seg_tag, "_3h.csv")
    )
    
    write_csv(pred, out_pred_file)
    
    # --------------------------------------------------------
    # 6.13 Save convergence summary
    # --------------------------------------------------------
    
    out_conv_file <- file.path(
      outdir,
      paste0("Balaenoptera_artificialis_L2_", seg_tag, "_convergence.csv")
    )
    
    write_csv(
      tibble(
        id = seg_tag,
        converged = fit$converged
      ),
      out_conv_file
    )
    
    # --------------------------------------------------------
    # 6.14 Compute a bounding box for the ROUTED segment only
    # --------------------------------------------------------
    # This will be used in the right QC panel.
    # --------------------------------------------------------
    
    xr_seg <- range(pred$lon, na.rm = TRUE)
    yr_seg <- range(pred$lat, na.rm = TRUE)
    
    if (diff(xr_seg) < 0.2) xr_seg <- xr_seg + c(-0.2, 0.2)
    if (diff(yr_seg) < 0.2) yr_seg <- yr_seg + c(-0.2, 0.2)
    
    bb_seg_xlim <- c(xr_seg[1] - 0.5, xr_seg[2] + 0.5)
    bb_seg_ylim <- c(yr_seg[1] - 0.5, yr_seg[2] + 0.5)
    
    # --------------------------------------------------------
    # 6.15 Build the LEFT QC panel
    # --------------------------------------------------------
    # Full raw track:
    # - all positions in red
    # - active segment in black
    # --------------------------------------------------------
    
    p_raw <- ggplot() +
      geom_sf(data = world, fill = "grey90", color = "grey40") +
      geom_path(
        data = df,
        aes(x = lon, y = lat),
        color = "red",
        linewidth = 1
      ) +
      geom_point(
        data = df,
        aes(x = lon, y = lat),
        color = "red",
        size = 1.5
      ) +
      geom_point(
        data = seg,
        aes(x = lon, y = lat),
        color = "black",
        size = 3
      ) +
      coord_sf(
        xlim = bb_raw_xlim,
        ylim = bb_raw_ylim,
        expand = FALSE
      ) +
      theme_bw() +
      labs(
        title = paste("Raw track -", tag_id),
        subtitle = paste("Active segment:", seg_name),
        x = "Longitude",
        y = "Latitude"
      )
    
    # --------------------------------------------------------
    # 6.16 Build the RIGHT QC panel
    # --------------------------------------------------------
    # Routed SSM output:
    # - line colored by behavioural index g
    # - points colored by behavioural index g
    # --------------------------------------------------------
    
    p_ssm <- ggplot() +
      geom_sf(data = world, fill = "grey90", color = "grey40") +
      geom_path(
        data = pred,
        aes(x = lon, y = lat, colour = g),
        linewidth = 0.9
      ) +
      geom_point(
        data = pred,
        aes(x = lon, y = lat, colour = g),
        size = 1.3
      ) +
      scale_colour_viridis_c(
        name = "Behaviour (g)",
        option = "plasma"
      ) +
      coord_sf(
        xlim = bb_seg_xlim,
        ylim = bb_seg_ylim,
        expand = FALSE
      ) +
      theme_bw() +
      labs(
        title = paste("SSM routed (3h) -", seg_tag),
        subtitle = "aniMotum mp model - continuous behavioural index",
        x = "Longitude",
        y = "Latitude"
      )
    
    # --------------------------------------------------------
    # 6.17 Combine and save the QC figure
    # --------------------------------------------------------
    
    p_final <- p_raw + p_ssm + plot_layout(ncol = 2)
    
    out_plot_file <- file.path(
      outdir,
      paste0("Balaenoptera_artificialis_QC_", seg_tag, ".png")
    )
    
    ggsave(
      filename = out_plot_file,
      plot = p_final,
      width = 12,
      height = 6,
      dpi = 300
    )
    
    message("    Saved outputs for ", seg_tag)
  }
}

# ============================================================
# 7. FINAL MESSAGE
# ============================================================

message("====================================")
message("SSM + behaviour + route_path completed")
message("Model: mp (behavioural)")
message("The script is robust to:")
message("  - optimiser failures")
message("  - short segments")
message("  - route_path failures")
message("Saved:")
message("  - one routed CSV per fitted segment")
message("  - one convergence CSV per fitted segment")
message("  - one QC plot per fitted segment")
message("====================================")