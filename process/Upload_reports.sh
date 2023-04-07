#!/bin/bash

#### 4. Upload data to different locations for other uses

info() { echo ; echo "$(date +'%F %T') ::${SCRIPT}::${ID}:: $* ::" ; echo ; }

info " R4.0 Upload results start"


## rclone options
bwlim=500  # if not set to 110
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s "
bwlimit=" --bwlimit=${bwlim}k "



echo "lapauththanasis:/Aerosols/test_graphs"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" sync "$HOME/Aerosols/DATA/Graphs"      lapauththanasis:/Aerosols/test_graphs

echo "lapauththanasis:/Aerosols/test_logs_stat"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" sync "$HOME/Aerosols/DATA/statistics"  lapauththanasis:/Aerosols/test_logs_stats

echo "lapauththanasis:/Aerosols/uwyo"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" sync "$HOME/DATA_RAW/uwyo"             lapauththanasis:/Aerosols/uwyo

echo "lapauththanasis:/Public"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" copy "$HOME/LOGs/weewx.sdb"            lapauththanasis:/Public
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" copy "$HOME/LOGs/LAP_AUTH_davis.csv"   lapauththanasis:/Public
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" copy "$HOME/LOGs/LAP_AUTH_davis.info"  lapauththanasis:/Public
"${rclone}" ${otheropt} ${bwlimit}  --config "$config" copy "$HOME/LOGs/LAP_AUTH_davis.md"    lapauththanasis:/Public


## sync Formal folder from sagan
echo "natsisthanasis:/Formal"
"${rclone}" ${otheropt} ${bwlimit}  --config "$config"   \
        --exclude "**/.git/"                             \
        --exclude  "*/.git/"                             \
        --exclude    ".git/**"                           \
        --exclude   "*.git/**"                           \
        --exclude    ".git/"                             \
        --exclude   "*.git/"                             \
        sync "$HOME/Formal"   "natsisthanasis:/Formal"


info " R4.0 Upload results end"
