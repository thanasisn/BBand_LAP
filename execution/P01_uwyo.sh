#!/usr/bin/env bash
## created on 2024-11-05

####  Get parse and upload LGTS sounding data from uwyo

exec 9>"/dev/shm/$(basename $0).lock"
if ! flock -n 9  ; then
    echo "another instance of $0 is running";
    exit 1
fi

ldir="$HOME/BBand_LAP/REPORTS/LOGs/uwyo"
mkdir -p "$ldir"
LOG_FILE="$ldir/$(basename "$0")_$(date +%F_%R).log"
ERR_FILE="$ldir/$(basename "$0")_$(date +%F_%R).err"
# exec  > >(tee -i "${LOG_FILE}")
# exec 2> >(tee -i "${ERR_FILE}" >&2)
## send output directly to files
exec  > "${LOG_FILE}"
exec 2> "${ERR_FILE}"
TIC=$(date +"%s")

: "${ID:=$(hostname)}"
SCRIPT="$(basename "$0")"

## rclone options
bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s "
bwlimit=" --bwlimit=${bwlim}k "

info() { echo ; echo "$(date +'%F %T') ::${SCRIPT}::${ID}:: $* ::" ; echo ; }

echo "###################################"
echo "####    $(date +"%F %T")    ####"
echo "###################################"

## ignore errors
set +e

info "##  Scrap uwyo data for Thessaloniki"
"$HOME/BBand_LAP/parameters/uwyo/scrap_uwyo.sh"

info "##  Parse uwyo ground data for Thessaloniki"
"$HOME/BBand_LAP/parameters/uwyo/parse_uwyo.R"

info "##  Upload uwyo data for Thessaloniki"
"${rclone}" ${otheropt} ${bwlimit} --config "$config" copy --include "LGTS_soundings.*" "$HOME/DATA/WEATHER/" "lapauththanasis:/Public"
"${rclone}" ${otheropt} ${bwlimit} --config "$config" sync "$HOME/DATA_RAW/uwyo" "lapauththanasis:/Public/uwyo"


info "#### END $0 ####"
TAC=$(date +"%s"); dura="$( echo "scale=6; ($TAC-$TIC)/60" | bc)"
printf "%s %-10s %-10s %-10s %f\n" "$(date +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$dura"
