#!/bin/bash
## created on 2023-04-03

#### Build the Broad Band database from scratch.

info() { echo ; echo "$(date +%F_%T) :: $* " >&1; }
mkdir -p "$(dirname "$0")/REPORTS/LOGs/"
LOG_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).log"
OUT_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).out"
ERR_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).err"
exec  > >(tee -i "${OUT_FILE}")
exec  > >(tee -i "${LOG_FILE}") 2>&1
exec 2> >(tee -i "${ERR_FILE}" >&2)
info "START :: $0 :: $* ::"

echo ""
echo " * *  STOP BE CAREFUL !!  * *"
echo ""
read -p "Do you want to DELETE the Broad Band database? " conf
echo ""
if   [[ $conf == "yes" ]]; then
    info "Remove Broad Band data and metadata"
    rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB"
    rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB_metadata.parquet"
    rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB.stopfile"
else
    echo "DO NOT REMOVED!"
fi

echo ""
read -p "Do you want to DELETE the CHP-1 tracker database? " conf
echo ""
if   [[ $conf == "yes" ]]; then
    info "Remove CHP-1 tracker data and metadata"
    rm -rfv "$HOME/DATA/Broad_Band/CHP1_Tracker_steps_DB"
    rm -rfv "$HOME/DATA/Broad_Band/CHP1_Tracker_steps_DB_metadata.parquet"
    rm -rfv "$HOME/DATA/Broad_Band/CHP1_Tracker_steps_DB.stopfile"
else
    echo "DO NOT REMOVED!"
fi


info "Run other processes"
"$HOME/BBand_LAP/process/Process_BB_DB.R"

info "Update Readme.md file"
"$HOME/BBand_LAP/.update_readme.sh"

exit 0
