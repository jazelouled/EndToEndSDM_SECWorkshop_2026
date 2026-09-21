# ============================================================
# Script name: 02_L1_douglas_speed_filter_Balaenoptera_artificialis_from_L0.R
# Description:
# Full L0 -> L1 pipeline for Balaenoptera artificialis tracking.
# Applies Douglas filter using ONLY a speed criterion:
#   vmax = 25 km/h
#
# Rules:
#  - Remove LocationClass == "Z"
#  - Remove positions falling on land using oceanmask.tif
#  - Apply Douglas speed-only filter to all remaining points
#  - No GEO_BBOX removal
#  - No angle / distance constraints
#
# Outputs:
#  - L1 filtered CSV per tag
#  - L1 with flags CSV per tag
#  - QC map per tag
#  - side-by-side plot per tag: before vs after
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(argosfilter)
  library(grid)
  library(patchwork)
  library(terra)
})

sf::sf_use_s2(FALSE)

message("Starting Douglas speed-only filtering workflow for Balaenoptera artificialis...")

# ============================================================
# Paths
# ============================================================

path_L0 <- here("00inputOutput", "00input", "01processedData", "01tracking", "00L0_data")
path_mask <- here("00inputOutput", "00input", "00rawData", "00enviro", "oceanmask.tif")
out_base <- here("00inputOutput", "00input", "01processedData", "01tracking", "02L1_douglas")

out_L1_dir      <- file.path(out_base, "L1_filtered")
out_flags_dir   <- file.path(out_base, "L1_withFlags")
out_plots_dir   <- file.path(out_base, "plots")
out_compare_dir <- file.path(out_plots_dir, "before_after_sideBySide")

dir.create(out_L1_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_flags_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_compare_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(path_L0)) stop("L0 directory not found: ", path_L0)
if (!file.exists(path_mask)) stop("Ocean mask not found: ", path_mask)

# ============================================================
# Parameters
# ============================================================

vmax_kmh <- 25
vmax_ms  <- vmax_kmh / 3.6

# Only for visualisation
xmin <- -6
xmax <- 16
ymin <- 30
ymax <- 46

# ============================================================
# Read mask
# ============================================================

message("Reading ocean mask...")
oceanmask <- terra::rast(path_mask)

# IMPORTANT:
# Adjust if your raster uses the opposite coding.
# Here we assume ocean = 0
ocean_values <- c(1)

# ============================================================
# Read all L0 files
# ============================================================

files_L0 <- list.files(
  path_L0,
  pattern = "^Balaenoptera_artificialis_L0_PTT_.*\\.csv$",
  full.names = TRUE
)

if (length(files_L0) == 0) {
  stop("No L0 files found in: ", path_L0)
}

message("Reading L0 files...")

L0_list <- files_L0 %>%
  set_names(basename(.)) %>%
  purrr::map(~ read_csv(
    .x,
    show_col_types = FALSE,
    col_types = cols(
      PTT_ID = col_character(),
      DateTime = col_character(),
      Latitude = col_double(),
      Longitude = col_double(),
      LocationClass = col_character()
    )
  ))

message("Loaded ", length(L0_list), " L0 files.")

# ============================================================
# Helper: extract ocean mask
# ============================================================

extract_ocean_mask <- function(df, mask_rast) {
  
  pts <- terra::vect(
    df %>% select(Longitude, Latitude),
    geom = c("Longitude", "Latitude"),
    crs = "EPSG:4326"
  )
  
  vals <- terra::extract(mask_rast, pts)
  df$mask_value <- vals[, 2]
  
  df
}

# ============================================================
# Douglas speed-only filter WITH REASONS
# ============================================================

apply_douglas_speed_only_with_reasons <- function(df, vmax_kmh = 25, mask_rast, ocean_values = c(0)) {
  
  vmax_ms <- vmax_kmh / 3.6
  
  df0 <- df %>%
    mutate(
      row_id = row_number(),
      DateTime = ymd_hms(DateTime, tz = "UTC"),
      LocationClass = as.character(LocationClass),
      douglas_flag  = NA_character_,
      douglas_keep  = NA,
      final_reason  = NA_character_
    )
  
  # ----------------------------------------------------------
  # Rule 1: remove LC Z
  # ----------------------------------------------------------
  
  df0 <- df0 %>%
    mutate(
      final_reason = case_when(
        LocationClass == "Z" ~ "LC_Z",
        TRUE ~ final_reason
      ),
      douglas_keep = case_when(
        LocationClass == "Z" ~ FALSE,
        TRUE ~ douglas_keep
      )
    )
  
  # ----------------------------------------------------------
  # Rule 2: remove points falling on land according to mask
  # ----------------------------------------------------------
  
  df0 <- df0 %>%
    filter(!is.na(Longitude), !is.na(Latitude))
  
  df0 <- extract_ocean_mask(df0, mask_rast)
  
  df0 <- df0 %>%
    mutate(
      is_ocean = !is.na(mask_value) & mask_value %in% ocean_values
    ) %>%
    mutate(
      final_reason = case_when(
        !is.na(final_reason) ~ final_reason,
        !is_ocean ~ "LAND_MASK",
        TRUE ~ final_reason
      ),
      douglas_keep = case_when(
        !is.na(final_reason) & final_reason %in% c("LC_Z", "LAND_MASK") ~ FALSE,
        TRUE ~ douglas_keep
      )
    )
  
  # ----------------------------------------------------------
  # Subset entering Douglas
  # ----------------------------------------------------------
  
  df_doug <- df0 %>%
    filter(is.na(final_reason)) %>%
    filter(
      !is.na(DateTime),
      !is.na(Longitude),
      !is.na(Latitude)
    ) %>%
    arrange(DateTime)
  
  # remove duplicated timestamps
  df_doug <- df_doug %>%
    distinct(DateTime, .keep_all = TRUE) %>%
    arrange(DateTime)
  
  # remove non-increasing times
  df_doug <- df_doug %>%
    mutate(
      dt_sec = as.numeric(difftime(DateTime, lag(DateTime), units = "secs"))
    ) %>%
    filter(is.na(dt_sec) | dt_sec > 0) %>%
    select(-dt_sec)
  
  # ----------------------------------------------------------
  # If too few points, keep valid ones as end_location
  # ----------------------------------------------------------
  
  if (nrow(df_doug) < 3) {
    
    if (nrow(df_doug) > 0) {
      df_doug <- df_doug %>%
        mutate(
          douglas_flag = "end_location",
          douglas_keep = TRUE,
          final_reason = "KEPT"
        )
    }
    
  } else {
    
    flag <- tryCatch(
      {
        sdafilter(
          lat     = df_doug$Latitude,
          lon     = df_doug$Longitude,
          dtime   = df_doug$DateTime,
          lc      = rep("3", nrow(df_doug)),
          vmax    = vmax_ms,
          ang     = c(0, 0),
          distlim = c(0, 0)
        )
      },
      error = function(e) {
        message("Douglas failed for one tag; valid points will be kept as end_location.")
        rep("end_location", nrow(df_doug))
      }
    )
    
    df_doug <- df_doug %>%
      mutate(
        douglas_flag = flag,
        douglas_keep = douglas_flag != "removed",
        final_reason = case_when(
          douglas_flag == "removed" ~ "DOUGLAS_SPEED",
          TRUE ~ "KEPT"
        )
      )
  }
  
  # ----------------------------------------------------------
  # Join back to full table
  # ----------------------------------------------------------
  
  df_out <- df0 %>%
    left_join(
      df_doug %>%
        select(row_id, douglas_flag, douglas_keep, final_reason),
      by = "row_id",
      suffix = c("", ".new")
    ) %>%
    mutate(
      douglas_flag = coalesce(douglas_flag.new, douglas_flag),
      douglas_keep = coalesce(douglas_keep.new, douglas_keep),
      final_reason = coalesce(final_reason.new, final_reason)
    ) %>%
    select(-douglas_flag.new, -douglas_keep.new, -final_reason.new)
  
  # ----------------------------------------------------------
  # Final safety rules
  # ----------------------------------------------------------
  
  df_out <- df_out %>%
    mutate(
      douglas_keep = case_when(
        is.na(douglas_keep) & is.na(final_reason) ~ FALSE,
        TRUE ~ douglas_keep
      ),
      final_reason = case_when(
        douglas_keep == TRUE ~ "KEPT",
        douglas_keep == FALSE & is.na(final_reason) ~ "DOUGLAS_SPEED",
        TRUE ~ final_reason
      )
    ) %>%
    arrange(DateTime)
  
  df_out
}

# ============================================================
# Apply to all tags
# ============================================================

message("Applying Douglas speed-only filter to all tags...")

L1_withFlags_list <- L0_list %>%
  purrr::map(~ apply_douglas_speed_only_with_reasons(
    .x,
    vmax_kmh = vmax_kmh,
    mask_rast = oceanmask,
    ocean_values = ocean_values
  ))

L1_filtered_list <- L1_withFlags_list %>%
  purrr::map(~ filter(.x, douglas_keep))

# ============================================================
# Save outputs
# ============================================================

message("Saving L1 outputs...")

walk(names(L1_withFlags_list), function(fname) {
  
  tag <- gsub("^Balaenoptera_artificialis_L0_|\\.csv$", "", fname)
  
  write_csv(
    L1_withFlags_list[[fname]],
    file.path(out_flags_dir, paste0("Balaenoptera_artificialis_L1_withFlags_", tag, ".csv"))
  )
  
  write_csv(
    L1_filtered_list[[fname]],
    file.path(out_L1_dir, paste0("Balaenoptera_artificialis_L1_", tag, ".csv"))
  )
})

# ============================================================
# Plotting helpers
# ============================================================

message("Preparing plotting layers...")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

world_crop <- st_crop(
  world,
  xmin = xmin, xmax = xmax,
  ymin = ymin, ymax = ymax
)

make_bbox <- function(df, buffer_deg = 0.5) {
  
  xr <- range(df$Longitude, na.rm = TRUE)
  yr <- range(df$Latitude, na.rm = TRUE)
  
  if (diff(xr) < 0.2) xr <- xr + c(-0.2, 0.2)
  if (diff(yr) < 0.2) yr <- yr + c(-0.2, 0.2)
  
  xlim <- c(xr[1] - buffer_deg, xr[2] + buffer_deg)
  ylim <- c(yr[1] - buffer_deg, yr[2] + buffer_deg)
  
  xlim[1] <- max(xlim[1], xmin)
  xlim[2] <- min(xlim[2], xmax)
  ylim[1] <- max(ylim[1], ymin)
  ylim[2] <- min(ylim[2], ymax)
  
  list(xlim = xlim, ylim = ylim)
}

quality_map <- c(
  "3" = "#1b9e77",
  "2" = "#66a61e",
  "1" = "#e6ab02",
  "0" = "#d95f02",
  "A" = "#7570b3",
  "B" = "#e7298a",
  "Z" = "#666666"
)

reason_colors <- c(
  "LC_Z" = "orange",
  "LAND_MASK" = "purple",
  "DOUGLAS_SPEED" = "red"
)

# ============================================================
# QC MAP per tag
# ============================================================

plot_douglas_qc <- function(df, tag_id) {
  
  df <- df %>%
    mutate(
      DateTime = as.POSIXct(DateTime, tz = "UTC"),
      LocationClass = factor(LocationClass, levels = names(quality_map)),
      final_reason  = factor(final_reason,
                             levels = c("KEPT", "LC_Z", "LAND_MASK", "DOUGLAS_SPEED"))
    ) %>%
    arrange(DateTime)
  
  df_plot <- df %>%
    filter(
      !is.na(Longitude),
      !is.na(Latitude),
      Longitude >= xmin, Longitude <= xmax,
      Latitude  >= ymin, Latitude  <= ymax
    )
  
  if (nrow(df_plot) == 0) return(NULL)
  
  n_total   <- nrow(df)
  n_kept    <- sum(df$douglas_keep, na.rm = TRUE)
  n_removed <- sum(!df$douglas_keep, na.rm = TRUE)
  
  n_lcz   <- sum(df$final_reason == "LC_Z", na.rm = TRUE)
  n_land  <- sum(df$final_reason == "LAND_MASK", na.rm = TRUE)
  n_speed <- sum(df$final_reason == "DOUGLAS_SPEED", na.rm = TRUE)
  
  bb <- make_bbox(df_plot)
  
  kept_df <- df_plot %>%
    filter(douglas_keep) %>%
    arrange(DateTime)
  
  removed_df <- df_plot %>%
    filter(!douglas_keep, final_reason != "KEPT")
  
  start_point <- kept_df %>% slice(1)
  
  subtitle_txt <- paste0(
    "vmax = ", vmax_kmh, " km/h | ",
    "Total: ", n_total,
    " | Kept: ", n_kept,
    " | Removed: ", n_removed,
    " | LC_Z: ", n_lcz,
    " | LAND_MASK: ", n_land,
    " | DOUGLAS_SPEED: ", n_speed
  )
  
  p <- ggplot() +
    geom_sf(data = world_crop, fill = "grey90", color = "grey40") +
    
    geom_path(
      data = kept_df,
      aes(x = Longitude, y = Latitude),
      color = "blue",
      linewidth = 0.6,
      alpha = 0.6
    ) +
    
    geom_point(
      data = removed_df,
      aes(x = Longitude, y = Latitude, color = final_reason),
      size = 2,
      alpha = 0.8
    ) +
    
    geom_point(
      data = kept_df,
      aes(x = Longitude, y = Latitude, fill = LocationClass),
      shape = 21,
      size = 1.2,
      alpha = 0.55,
      color = "black",
      stroke = 0.15
    ) +
    
    {
      if (nrow(start_point) > 0)
        geom_point(
          data = start_point,
          aes(x = Longitude, y = Latitude),
          color = "green3",
          size = 3
        )
    } +
    
    scale_fill_manual(
      values = quality_map,
      limits = names(quality_map),
      drop = FALSE,
      na.value = "grey70"
    ) +
    
    scale_color_manual(
      values = reason_colors,
      breaks = c("LC_Z", "LAND_MASK", "DOUGLAS_SPEED"),
      drop = FALSE
    ) +
    
    guides(
      fill = guide_legend(
        override.aes = list(
          shape = 21,
          size = 2.5,
          alpha = 1,
          color = "black"
        )
      ),
      color = guide_legend(
        override.aes = list(
          size = 3,
          alpha = 1
        )
      )
    ) +
    
    coord_sf(xlim = bb$xlim, ylim = bb$ylim, expand = FALSE) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      plot.subtitle = element_text(size = 9)
    ) +
    labs(
      title = paste("Douglas QC -", tag_id),
      subtitle = subtitle_txt,
      x = "Longitude",
      y = "Latitude",
      fill = "Quality",
      color = "Removed reason"
    )
  
  ggsave(
    filename = file.path(out_plots_dir, paste0("Douglas_QC_", tag_id, ".png")),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# BEFORE vs AFTER side-by-side plot
# ============================================================

plot_before_after <- function(df, tag_id) {
  
  df <- df %>%
    mutate(
      DateTime = as.POSIXct(DateTime, tz = "UTC"),
      LocationClass = factor(LocationClass, levels = names(quality_map)),
      final_reason  = factor(final_reason,
                             levels = c("KEPT", "LC_Z", "LAND_MASK", "DOUGLAS_SPEED"))
    ) %>%
    arrange(DateTime)
  
  df_plot <- df %>%
    filter(
      !is.na(Longitude),
      !is.na(Latitude),
      Longitude >= xmin, Longitude <= xmax,
      Latitude  >= ymin, Latitude  <= ymax
    )
  
  if (nrow(df_plot) == 0) return(NULL)
  
  bb <- make_bbox(df_plot)
  
  before_df <- df_plot %>% arrange(DateTime)
  after_df  <- df_plot %>% filter(douglas_keep) %>% arrange(DateTime)
  
  start_before <- before_df %>% slice(1)
  start_after  <- after_df %>% slice(1)
  
  p_before <- ggplot() +
    geom_sf(data = world_crop, fill = "grey90", color = "grey40") +
    geom_path(
      data = before_df,
      aes(x = Longitude, y = Latitude),
      color = "grey45",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    geom_point(
      data = before_df,
      aes(x = Longitude, y = Latitude, fill = LocationClass),
      shape = 21,
      size = 1.2,
      alpha = 0.55,
      color = "black",
      stroke = 0.15
    ) +
    {
      if (nrow(start_before) > 0)
        geom_point(
          data = start_before,
          aes(x = Longitude, y = Latitude),
          color = "green3",
          size = 3
        )
    } +
    scale_fill_manual(
      values = quality_map,
      limits = names(quality_map),
      drop = FALSE,
      na.value = "grey70"
    ) +
    coord_sf(xlim = bb$xlim, ylim = bb$ylim, expand = FALSE) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12)
    ) +
    labs(
      title = "Before Douglas filtering",
      x = "Longitude",
      y = "Latitude"
    )
  
  p_after <- ggplot() +
    geom_sf(data = world_crop, fill = "grey90", color = "grey40") +
    geom_path(
      data = after_df,
      aes(x = Longitude, y = Latitude),
      color = "blue",
      linewidth = 0.6,
      alpha = 0.6
    ) +
    geom_point(
      data = after_df,
      aes(x = Longitude, y = Latitude, fill = LocationClass),
      shape = 21,
      size = 1.2,
      alpha = 0.55,
      color = "black",
      stroke = 0.15
    ) +
    {
      if (nrow(start_after) > 0)
        geom_point(
          data = start_after,
          aes(x = Longitude, y = Latitude),
          color = "green3",
          size = 3
        )
    } +
    scale_fill_manual(
      values = quality_map,
      limits = names(quality_map),
      drop = FALSE,
      na.value = "grey70"
    ) +
    coord_sf(xlim = bb$xlim, ylim = bb$ylim, expand = FALSE) +
    theme_bw() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 12),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.4, "cm")
    ) +
    labs(
      title = "After Douglas filtering",
      x = "Longitude",
      y = "Latitude",
      fill = "Quality"
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          shape = 21,
          size = 2.5,
          alpha = 1,
          color = "black"
        )
      )
    )
  
  p_final <- p_before | p_after
  
  ggsave(
    filename = file.path(out_compare_dir, paste0("Douglas_beforeAfter_", tag_id, ".png")),
    plot = p_final,
    width = 12,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# Generate all QC plots
# ============================================================

message("Generating QC plots...")

walk(names(L1_withFlags_list), function(fname) {
  
  tag <- gsub("^Balaenoptera_artificialis_L0_|\\.csv$", "", fname)
  df  <- L1_withFlags_list[[fname]]
  
  plot_douglas_qc(df, tag)
  plot_before_after(df, tag)
})

message("====================================")
message("Douglas speed-only filter completed")
message("vmax = ", vmax_kmh, " km/h")
message("Land-mask removal applied with oceanmask.tif")
message("IMPORTANT: check that ocean_values matches your raster coding")
message("Saved:")
message("- L1 filtered CSV per tag")
message("- L1 with flags CSV per tag")
message("- QC map per tag")
message("- before/after comparison per tag")
message("====================================")