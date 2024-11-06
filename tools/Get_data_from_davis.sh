#!/usr/bin/env bash

####  Store data from LAP Davis

if [[ $(hostname) != "sagan" ]]; then
    echo "Run this from sagan only"
fi

ldir="$HOME/BBand_LAP/REPORTS/LOGs/Get_LAP"
mkdir -p "$ldir"
LOG_FILE="$ldir/$(basename "$0")_$(date +%F_%R).log"
ERR_FILE="$ldir/$(basename "$0")_$(date +%F_%R).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)

SOURCE_DB="/var/lib/weewx/weewx.sdb"
TARGET_DB="$HOME/DATA_RAW/LAPWeath/LAP_roof/LAP_AUTH_davis.sdb"
TARGET_FL="$HOME/DATA_RAW/LAPWeath/LAP_roof/LAP_AUTH_davis.csv"
CONFIG_DR="$HOME/DATA_RAW/LAPWeath/LAP_roof/weewx_etc"

##  Copy Davis data
cp -auv              "$SOURCE_DB"                            "$TARGET_DB"

##  Dump DB to CSV
sqlite3 -header -csv "$TARGET_DB" "select * from archive;" > "$TARGET_FL"

##  Copy configurations
rsync -rah           "/etc/weewx/"                           "$CONFIG_DR"

##  Upload
bwlim=500  # if not set to 110
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s "
bwlimit=" --bwlimit=${bwlim}k "

"${rclone}" ${otheropt} ${bwlimit} --config "$config" copy "$TARGET_DB"                                          "lapauththanasis:/Public/LAP_Davis"
"${rclone}" ${otheropt} ${bwlimit} --config "$config" copy "$TARGET_FL"                                          "lapauththanasis:/Public/LAP_Davis"
"${rclone}" ${otheropt} ${bwlimit} --config "$config" copy "$HOME/DATA_RAW/LAPWeath/LAP_roof/LAP_AUTH_davis.md"  "lapauththanasis:/Public/LAP_Davis"

exit 0
