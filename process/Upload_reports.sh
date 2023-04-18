#!/bin/bash

#### 4. Upload data to different locations for other uses

info() { echo ; echo "$(date +'%F %T') ::${SCRIPT}::${ID}:: $* ::" ; echo ; }

info "Upload results start"

## rclone options
bwlim=1000  # if not set to 110
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s "
bwlimit=" --bwlimit=${bwlim}k "

if [[ $(hostname) != "sagan" ]]; then
    echo "This should run only by sagan"
    exit 11
fi

echo "lapauththanasis:/Aerosols/test_graphs"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" sync "$HOME/BBand_LAP/REPORTS"   lapauththanasis:/BroadBand/

info "Upload results end"

