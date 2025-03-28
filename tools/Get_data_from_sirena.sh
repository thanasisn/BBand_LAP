#!/usr/bin/env bash

#### Get data from sirena and commit to github.

ldir="$HOME/BBand_LAP/REPORTS/LOGs/Get_LAP"
mkdir -p "$ldir"
LOG_FILE="$ldir/$(basename "$0")_$(date +%F_%R).log"
ERR_FILE="$ldir/$(basename "$0")_$(date +%F_%R).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)

##  Run rsync if mounted  ------------------------------------------------------
SOURCE="/media/sirena_lapdata_ro"
if mountpoint -q "$SOURCE" ; then
  echo "Sirena is mounted"
else
  echo ""
  echo "No DATA mount point found!!"
  notify-send          -u critical "1: SIRENA NOT MOUNTED" "Can not get source files"
  pub_notifications.py -u critical "2: SIRENA NOT MOUNTED" "Can not get source files"
fi

##  Get data  ------------------------------------------------------------------

##  Get all broadband channels

target_base="$HOME/DATA_RAW/Bband/"
## use full paths
folders=(
	"/media/sirena_lapdata_ro/archive/Bband/TUVR_LAP.GLB/"   # 0
	"/media/sirena_lapdata_ro/archive/Bband/CM21_LAP.INC/"   # 1
	"/media/sirena_lapdata_ro/archive/Bband/PIR_lap.GLB/"    # 2
	"/media/sirena_lapdata_ro/archive/Bband/CHP1_lap.DIR/"   # 3
	"/media/sirena_lapdata_ro/archive/Bband/YES_LAP.GLB/"    # 4
	"/media/sirena_lapdata_ro/archive/Bband/EKO_LAP.GLB/"    # 5
	"/media/sirena_lapdata_ro/archive/Bband/AC21_LAP.GLB/"   # 6
	"/media/sirena_lapdata_ro/archive/Bband/YES_LAP.DIF/"    # 7
)

for from in "${folders[@]}"; do
  ## test source and create target
  target="$target_base/$(basename "$from")"
  [ ! -d "$from" ] && echo "Folder not there: $from" && continue

  echo
  echo "-------------------------------------------------------------------"
  echo "  Doing: $from"
  echo "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
  echo "$from -> $target"

  rsync -arvth          \
    --include '*/'      \
    --include '*.LAP'   \
    --include '*.lap'   \
    --include '*.ori'   \
    --exclude '*'       \
    "$from/"            \
    "$target"
done

echo "- - - - - - - - - - - - - - - - - - - -"
echo "Get total radiation files 'TOT.DAT'"
rsync -arvth                                 \
  --delete                                   \
  "$SOURCE/products/Bband/AC21_lap.GLB/"     \
  "$HOME/DATA/cm21_data_validation/AC21_lap.GLB_TOT"

echo "- - - - - - - - - - - - - - - - - - - -"
echo "get other relative files"
rsync -arvth                                 \
  --include '*/'                             \
  --include '*.txt'                          \
  --include '*.bas'                          \
  --include '*.BAS'                          \
  --include '*.doc'                          \
  --include '*.docx'                         \
  --include '*.dat'                          \
  --include '*.xls'                          \
  --include '*.xlsx'                         \
  --exclude '*'                              \
  --prune-empty-dirs                         \
  "$SOURCE/process/"                         \
  "$HOME/DATA/process_sirena/"

echo "- - - - - - - - - - - - - - - - - - - -"
echo "get Brewer files"
rsync -arvth -u                              \
  "$SOURCE/products/Dsc"                     \
  "$HOME/DATA_RAW/Brewer_005"

echo "- - - - - - - - - - - - - - - - - - - -"
echo "get Sky camera files"
## Don't need to wait for this
(
rsync -arvth                                \
  --exclude "Thumns.db"                     \
  "$SOURCE/products/skycam"                 \
  "/home/single/LAP_skycam/"
rsync -arvth                                \
  --exclude "Thumns.db"                     \
  "$SOURCE/products/skycam_old"             \
  "/home/single/LAP_skycam/"
) &

##  Commit data to github to preserve manual edits  ----------------------------
folders=(
  "$HOME/DATA_RAW/Bband"
  "$HOME/DATA_RAW/tracker_chp1"
  "$HOME/DATA_RAW/Raddata"
)

for i in "${folders[@]}"; do
  echo
  [ ! -d "$i" ] && echo "Not a folder: $i" && continue
  ## get into the git folder
  cd "$i" || return
  pwd
  ## add files we care about
  find . -type f -not -path '*/\.git/*' -print0 |\
         xargs -0 git add -f
  ## commit and push
  git commit -uno -a -m "Commit $(date +'%F %R')"
  git push -f
  git push --tag
  git maintenance run --auto
done

##  Incremental copy in case of deleted files from source location  ------------
# rsync -ar --exclude='.git/' "$HOME/DATA_RAW/Bband/"        "$HOME/DATA_RAW/.Bband_capture"
# rsync -ar --exclude='.git/' "$HOME/DATA_RAW/tracker_chp1/" "$HOME/DATA_RAW/.tracker_chp1_capture"
# rsync -ar --exclude='.git/' "$HOME/DATA_RAW/Raddata"       "$HOME/DATA_RAW/.Raddata"

echo
echo "  ---------------------"
echo "  -- SIRENA SYNC END --"
echo "  ---------------------"
exit 0
