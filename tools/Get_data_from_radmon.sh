#!/usr/bin/env bash

#### Get data from radmon.

ldir="$HOME/BBand_LAP/REPORTS/LOGs/Get_LAP"
mkdir -p "$ldir"
LOG_FILE="$ldir/$(basename "$0")_$(date +%F_%R).log"
ERR_FILE="$ldir/$(basename "$0")_$(date +%F_%R).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)

TRACKER_MOUNT="/media/tracker_CHP1_rw/"
TRACKER_STORE="/home/athan/DATA_RAW/"

##  Run rsync if mounted  ------------------------------------------------------
SOURCE="/media/raddata"
if mountpoint -q "$SOURCE" ; then
  echo "Radmon is mounted"
else
    echo ""
    echo "No DATA mount point found!!"
    notify-send          -u critical "1: SIRENA NOT MOUNTED" "Can not get source files"
    pub_notifications.py -u critical "2: SIRENA NOT MOUNTED" "Can not get source files"
    exit 1
fi

##  Get data  ------------------------------------------------------------------

##
rsync -avhr "$SOURCE/8" "${TRACKER_STORE}/Raddata/"
##
rsync -avhr "$SOURCE/7" "${TRACKER_STORE}/Raddata/"

##  CM-21 TOT  ##
rsync -avhr "$SOURCE/6" "${TRACKER_STORE}/Raddata/"

##
rsync -avhr "$SOURCE/5" "${TRACKER_STORE}/Raddata/"
##
rsync -avhr "$SOURCE/4" "${TRACKER_STORE}/Raddata/"

##  CHP1 DIR   ##
rsync -avhr "$SOURCE/3" "${TRACKER_STORE}/Raddata/"

##  EPPLEY-IR  ##
rsync -avhr "$SOURCE/2" "${TRACKER_STORE}/Raddata/"

##  CM-21 INC  ##
rsync -avhr "$SOURCE/1" "${TRACKER_STORE}/Raddata/"

##  Get running code  ----------------------------------------------------------
rsync -avhr "${TRACKER_MOUNT}/source/"   "/home/athan/Aerosols/source_production/"
rsync -avhr "${TRACKER_MOUNT}/Aerosols"  "/home/athan/Aerosols/Dell/"

##  Get tracker and temperature data  ------------------------------------------
rsync -avhr "${TRACKER_MOUNT}/tracker_LOGs"  "${TRACKER_STORE}/tracker_chp1/"
rsync -avhr "${TRACKER_MOUNT}/tracker_SYNC"  "${TRACKER_STORE}/tracker_chp1/"
rsync -avhr "${TRACKER_MOUNT}/Tracker_STEP"  "${TRACKER_STORE}/tracker_chp1/"
rsync -avhr "${TRACKER_MOUNT}/Tracker_THERM" "${TRACKER_STORE}/tracker_chp1/"

echo
echo "  ---------------------"
echo "  -- RADMON SYNC END --"
echo "  ---------------------"
exit 0
