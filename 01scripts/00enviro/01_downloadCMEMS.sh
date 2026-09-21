#!/bin/bash
set -e

# ============================================================
# Download Copernicus data ONLY for tracking dates
# Simulated species – workshop example
# Bounding box loaded automatically from file
# ============================================================

# ----------------------------
# OPTIONAL: add Copernicus Marine executable to PATH
# Uncomment and adapt if needed
# ----------------------------
# Try to use copernicusmarine from PATH
if ! command -v copernicusmarine >/dev/null 2>&1; then
  echo "copernicusmarine not found in PATH, trying common conda locations..."

  for p in "$HOME/anaconda3/bin" "$HOME/miniconda3/bin" "$HOME/opt/anaconda3/bin" "$HOME/opt/miniconda3/bin"; do
    if [ -f "$p/copernicusmarine" ]; then
      export PATH="$p:$PATH"
      echo "Found copernicusmarine in $p"
      break
    fi
  done
fi

# Final check
if ! command -v copernicusmarine >/dev/null 2>&1; then
  echo "ERROR: copernicusmarine not found."
  echo "Please install Copernicus Marine Toolbox or add it to your PATH."
  exit 1
fi

# ----------------------------
# BASE DIRECTORY
# ----------------------------
# BASE_DIR="/Users/jazelouled-cheikhbonan/Dropbox/2026_ECS_WorkshopSDM/EndToEndSDM_ECSWorkshop_2026"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BASE_DIR="$( realpath "${SCRIPT_DIR}/../.." )"
echo "Using BASE_DIR:"
echo "$BASE_DIR"

DATES_FILE="${BASE_DIR}/00inputOutput/00input/00rawData/01tracking/00auxiliaryFiles/tracking_dates.txt"
BBOX_FILE="${BASE_DIR}/00inputOutput/00input/00rawData/01tracking/00auxiliaryFiles/bbox_env.txt"
OUTDIR="${BASE_DIR}/00inputOutput/00input/00rawData/00enviro/01CMEMS"

mkdir -p "${OUTDIR}"

# ----------------------------
# LOAD BBOX FROM FILE
# bbox_env.txt should contain:
# MIN_LON=...
# MAX_LON=...
# MIN_LAT=...
# MAX_LAT=...
# ----------------------------
source "${BBOX_FILE}"

echo "Using BBOX:"
echo "LON: ${MIN_LON} to ${MAX_LON}"
echo "LAT: ${MIN_LAT} to ${MAX_LAT}"

# ----------------------------
# DEPTHS (surface)
# ----------------------------
PHY_DEPTH=0.49402499198913574
BGC_DEPTH=0.5057600140571594

# ============================================================
# LOOP OVER DATES
# ============================================================

while read DATE; do

  echo "=============================================="
  echo "Processing date: ${DATE}"
  echo "=============================================="

  START="${DATE}T00:00:00"
  END="${DATE}T23:59:59"

  # ----------------------------
  # PHYSICAL VARIABLES
  # ----------------------------
  copernicusmarine subset \
    --dataset-id cmems_mod_glo_phy_my_0.083deg_P1D-m \
    --variable mlotst \
    --variable zos \
    --variable thetao \
    --variable so \
    --variable uo \
    --variable vo \
    --start-datetime ${START} \
    --end-datetime ${END} \
    --minimum-longitude ${MIN_LON} \
    --maximum-longitude ${MAX_LON} \
    --minimum-latitude ${MIN_LAT} \
    --maximum-latitude ${MAX_LAT} \
    --minimum-depth ${PHY_DEPTH} \
    --maximum-depth ${PHY_DEPTH} \
    --output-directory "${OUTDIR}"

  # ----------------------------
  # BIOGEOCHEMICAL VARIABLES
  # ----------------------------
  copernicusmarine subset \
    --dataset-id cmems_mod_glo_bgc_my_0.25deg_P1D-m \
    --variable chl \
    --variable nppv \
    --start-datetime ${START} \
    --end-datetime ${END} \
    --minimum-longitude ${MIN_LON} \
    --maximum-longitude ${MAX_LON} \
    --minimum-latitude ${MIN_LAT} \
    --maximum-latitude ${MAX_LAT} \
    --minimum-depth ${BGC_DEPTH} \
    --maximum-depth ${BGC_DEPTH} \
    --output-directory "${OUTDIR}"

done < "${DATES_FILE}"

echo "=============================================="
echo "DONE"
echo "Environmental data downloaded only for tracking dates"
echo "Output directory: ${OUTDIR}"
echo "=============================================="