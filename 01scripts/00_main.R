# ============================================================
# SCRIPT NAME:
# 00_main.R
#
# PURPOSE:
# Run the full End-to-End SDM workshop workflow in order,
# using project-relative paths through here().
#
# NOTES:
# - This script assumes it is run from inside the R project
# - Scripts are sourced in sequence
# - If one script fails, execution stops with an error
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGE
# ============================================================

suppressPackageStartupMessages({
  library(here)
})

message("Starting full workflow...")
message("Project root detected by here(): ", here::here())


# ============================================================
# CREATE RAW DATA FOLDERS (ENTRY POINT)
# ============================================================

suppressPackageStartupMessages({
  library(here)
})

message("Creating raw data input folders...")

dirs_to_create <- c(
  "00inputOutput/00input/00rawData/00enviro/00StaticLayers",
  "00inputOutput/00input/00rawData/01tracking"
)

for (d in dirs_to_create) {
  dir.create(here::here(d), recursive = TRUE, showWarnings = FALSE)
}

message("Raw data folders ready:")
print(here::here(dirs_to_create))


# ============================================================
# 2. DEFINE SCRIPT ORDER
# ============================================================
# All paths are relative to the project root
# ============================================================

scripts_to_run <- c(
  "01scripts/00enviro/000_makeAuxiliaryFiles.R",
  "01scripts/00enviro/00_oceanMask.R",
  "01scripts/00enviro/01_downloadCMEMS.R",
  "01scripts/00enviro/02_prepareStaticLayers.R",
  "01scripts/00enviro/03_prepareCMEMS.R",
  "01scripts/00enviro/04_buildPresentStack.R",
  
  "01scripts/01tracking/00_L0_read_and_standardize_Balaenoptera_artificialis_tracking.R",
  "01scripts/01tracking/01_L0_spaceTime_histograms_Balaenoptera_artificialis.R",
  "01scripts/01tracking/02_L1_douglas_speed_filter_Balaenoptera_artificialis_from_L0.R",
  "01scripts/01tracking/03_L1_spacetime_split_Balaenoptera_artificialis.R",
  "01scripts/01tracking/04_L2_ssm_by_segment_Balaenoptera_artificialis_QC_routePath.R",
  "01scripts/01tracking/05_simulations_tracks_Balaenoptera_artificialis.R",
  "01scripts/01tracking/06_presAbs_grid_balancing_Balaenoptera_artificialis.R",
  "01scripts/01tracking/07_extractEnvironmentalData_Balaenoptera_artificialis.R",
  
  "01scripts/02habitatModel/00_exploratoryDataAnalysis_Balaenoptera_artificialis.R",
  "01scripts/02habitatModel/01_fitRF_Balaenoptera_artificialis.R",
  "01scripts/02habitatModel/99sessionInfo.R"
)


# ============================================================
# 3. CHECK THAT ALL SCRIPTS EXIST
# ============================================================

full_paths <- file.path(here::here(), scripts_to_run)

missing_scripts <- full_paths[!file.exists(full_paths)]

if (length(missing_scripts) > 0) {
  stop(
    "The following scripts were not found:\n",
    paste(missing_scripts, collapse = "\n")
  )
}


# ============================================================
# 4. RUN SCRIPTS ONE BY ONE
# ============================================================

for (i in seq_along(scripts_to_run)) {
  
  rel_script <- scripts_to_run[i]
  full_script <- full_paths[i]
  
  message("====================================")
  message("Running script ", i, " of ", length(scripts_to_run))
  message("Script: ", rel_script)
  message("====================================")
  
  tryCatch(
    {
      source(full_script, local = FALSE)
      message("Finished: ", rel_script)
    },
    error = function(e) {
      stop(
        "\nWorkflow stopped while running:\n",
        rel_script,
        "\n\nError message:\n",
        conditionMessage(e)
      )
    }
  )
}


# ============================================================
# 5. FINAL MESSAGE
# ============================================================

message("====================================")
message("FULL WORKFLOW COMPLETED SUCCESSFULLY")
message("====================================")