#!/bin/bash

#### Get data from sirena and commit to github.
## Radmon is synced elsewhere

## Run rsync if mounted ----------------------------------------------
SOURCE="/media/sirena_lapdata_ro"
if mountpoint -q "$SOURCE" ; then

    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "Get signal files 'LAP' of CM-21 global"
    rsync -arvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/AC21_LAP.GLB/"      \
        "$HOME/DATA_RAW/Bband/AC21_LAP.GLB"

    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "Get signal files 'LAP' of CM-21 inclined"
    rsync -arvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/CM21_LAP.INC/"      \
        "$HOME/DATA_RAW/Bband/CM21_LAP.INC"

    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "Get signal files 'LAP' of ECO UVA? inclined"
    rsync -arvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/EKO_LAP.GLB/"       \
        "$HOME/DATA_RAW/Bband/EKO_LAP.GLB"

    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "Get CHP1 signal files"
    rsync -arvt                                    \
        "$SOURCE/archive/Bband/CHP1_lap.DIR/"      \
        "$HOME/DATA_RAW/Bband/CHP1_lap.DIR"

    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "Get total radiation files 'TOT.DAT'"
    rsync -arvt                                    \
        --delete                                   \
        "$SOURCE/products/Bband/AC21_lap.GLB/"     \
        "$HOME/DATA/cm21_data_validation/AC21_lap.GLB_TOT"


    echo "- - - - - - - - - - - - - - - - - - - -"
    echo "get other relative files"
    rsync -arvt                                    \
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
else
    echo ""
    echo "No DATA mount point found!!"
    notify-send          -u critical "1: SIRENA NOT MOUNTED" "Can not get source files"
    pub_notifications.py -u critical "2: SIRENA NOT MOUNTED" "Can not get source files"
fi


## Commit data to github to preserve manual edits --------------------
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


## Incremental copy in case of deleted files from source location ----
rsync -ar --exclude='.git/' "$HOME/DATA_RAW/Bband/"        "$HOME/DATA_RAW/.Bband_capture"
rsync -ar --exclude='.git/' "$HOME/DATA_RAW/tracker_chp1/" "$HOME/DATA_RAW/.tracker_chp1_capture"
rsync -ar --exclude='.git/' "$HOME/DATA_RAW/Raddata"       "$HOME/DATA_RAW/.Raddata"


echo
echo "  ---------------------"
echo "  -- FIN SIRENA SYNC --"
echo "  ---------------------"
exit 0
