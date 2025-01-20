#!/usr/bin/env bash
## created on 2024-11-05

#### Start the daily execution of Broadband dependent scripts

exec 9>"/dev/shm/$(basename $0).lock"
if ! flock -n 9  ; then
    echo "another instance of $0 is running";
    exit 1
fi

ldir="$HOME/BBand_LAP/REPORTS/LOGs/$(basename "$0")"
mkdir -p "$ldir"
LOG_FILE="$ldir/$(basename "$0")_$(date +%F_%R).log"
ERR_FILE="$ldir/$(basename "$0")_$(date +%F_%R).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)
TIC=$(date +"%s")

: "${ID:=$(hostname)}"
SCRIPT="$(basename "$0")"

info() { echo ; echo "$(date +'%F %T') ::${SCRIPT}::${ID}:: $* ::" ; echo ; }

echo "###################################"
echo "####    $(date +"%F %T")    ####"
echo "###################################"

## ignore errors
set +e
pids=()

(
  info "##  Start uwyo  ##"
  "$HOME/BBand_LAP/execution/P01_uwyo.sh"
  info "##  End uwyo STATUS:$?  ##"
) &

(
  sleep 1
  info "##  Start Get source files from Sirena  ##"
  "$HOME/BBand_LAP/tools/Get_data_from_sirena.sh"
  info "##  End Get source files from Sirena STATUS:$?  ##"
) & pids+=($!)

(
  sleep 3
  info "##  Start Get source files from Radmon  ##"
  "$HOME/BBand_LAP/tools/Get_data_from_radmon.sh"
  info "##  End Get source files from Radmon STATUS:$?  ##"
) & pids+=($!)

(
  sleep 5
  info "##  Start Get data from davis  ##"
  "$HOME/BBand_LAP/tools/Get_data_from_davis.sh"
  info "##  End Get data from davis STATUS:$?  ##"
) & pids+=($!)

(
  sleep 6
  info "##  Prepare TSI data  ##"
  "$HOME/BBand_LAP/parameters/TSI/Build_TSI.R"
  info "##  End Prepare TSI data STATUS:$?  ##"
) & pids+=($!)

(
  sleep 6
  info "##  Prepare weather data  ##"
  "$HOME/BBand_LAP/parameters/weather/Build_weather.R"
  info "##  End Prepare weather data STATUS:$?  ##"
) & pids+=($!)




## wait all the above to finish
wait "${pids[@]}"; pids=()


info "##  Start build_duckdb  ##"
"$HOME/BBand_LAP/build_duckdb/Build_BB_DB.R"
info "##  End build_duckdb STATUS:$?  ##"


info "##  Start QCRad LongShi  ##"
"$HOME/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_run.R"
info "##  End QCRad LongShi STATUS:$?  ##"


info "##  Start QCRad ThanasisN  ##"
"$HOME/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_run.R"
info "##  End QCRad ThanaisN STATUS:$?  ##"


info "##  Start inspect_duckdb  ##"
"$HOME/BBand_LAP/inspect_duckdb/Inspect_BB_DB.R"
info "##  End Inspect_BB_DB.R STATUS:$?  ##"


info "##  Start CSid_RenoHansen_run.R  ##"
"$HOME/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_run.R"
info "##  End CSid_RenoHansen_run.R STATUS:$?  ##"


info "##  Start other process  ##"
"$HOME/BBand_LAP/process/Process_BB_DB.R"
info "##  End Process_BB_DB.R STATUS:$?  ##"


info "##  Upload results  ##"
"$HOME/BBand_LAP/tools/Upload_reports.sh"
info "##  Upload results STATUS:$?  ##"


info "#### END $0 ####"
TAC=$(date +"%s"); dura="$( echo "scale=6; ($TAC-$TIC)/60" | bc)"
printf "%s %-10s %-10s %-10s %f\n" "$(date +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$dura"
exit 0
