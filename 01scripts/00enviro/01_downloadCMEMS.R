# =========================================================
# SCRIPT: 01_downloadCMEMS.sh
# PURPOSE:
# Execute the shell script that downloads daily CMEMS
# environmental data for the simulated species tracking dates.
#
# INPUT:
# 01scripts/00enviro/10downloadCMEMS.sh
#
# OUTPUT:
# 00inputOutput/00input/00rawData/00CMEMS/
# Make sure the PATH PART OF THE .sh file is OK
# =========================================================

message("Starting CMEMS download workflow...")

system(
  "bash 01scripts/00enviro/01_downloadCMEMS.sh",
  intern = FALSE,
  wait = TRUE
)

message("CMEMS download completed successfully.")