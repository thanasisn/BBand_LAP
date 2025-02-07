#!/bin/bash

#### Upload data to different locations for other uses

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

## rclone options
bwlim=1000  # if not set to 110
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s "
bwlimit=" --bwlimit=${bwlim}k "

if [[ $(hostname) != "sagan" ]]; then
    echo "This should run only by sagan"
    exit 9
fi

echo "lapauththanasis:/BroadBand"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" sync "$HOME/BBand_LAP/REPORTS"   lapauththanasis:/BroadBand/



info "#### END $0 ####"
TAC=$(date +"%s"); dura="$( echo "scale=6; ($TAC-$TIC)/60" | bc)"
printf "%s %-10s %-10s %-10s %f\n" "$(date +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$dura"
exit 0
