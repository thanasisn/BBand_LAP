#!/bin/bash

#### Get data from sirena.

## this sould be mounted
SOURCE="/media/sirena_lapdata_ro"


if mountpoint -q "$SOURCE" ; then

    echo "get signal files 'LAP' of CM-21 global"
    rsync -rhvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/AC21_LAP.GLB/"      \
        "$HOME/DATA_RAW/Bband/AC21_LAP.GLB"

    echo "get signal files 'LAP' of CM-21 inclined"
    rsync -rhvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/CM21_LAP.INC/"      \
        "$HOME/DATA_RAW/Bband/CM21_LAP.INC"

    echo "get signal files 'LAP' of ECO UVA? inclined"
    rsync -rhvt                                    \
        --include '*/'                             \
        --include '*.LAP'                          \
        --include '*.lap'                          \
        --include '*.ori'                          \
        --exclude '*'                              \
        "$SOURCE/archive/Bband/EKO_LAP.GLB/"       \
        "$HOME/DATA_RAW/Bband/EKO_LAP.GLB"

    echo "get CHP1 signal files"
    rsync -rhvt                                    \
        "$SOURCE/archive/Bband/CHP1_lap.DIR/"      \
        "$HOME/DATA_RAW/Bband/CHP1_lap.DIR"


    echo "get total radiation files 'TOT.DAT'"
    rsync -rhvt                                    \
        --delete                                   \
        "$SOURCE/products/Bband/AC21_lap.GLB/"     \
        "$HOME/DATA/cm21_data_validation/AC21_lap.GLB_TOT"


    echo "get other relative files"
    rsync -rhvt                                    \
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
fi

## commit data to git
folders=(
    "$HOME/DATA_RAW/Bband"
    "$HOME/DATA_RAW/tracker_chp1"
)

for i in "${folders[@]}"; do
    echo
    info " $i "
    echo
    [ ! -d "$i" ] && echo "Not a folder: $i" && continue
    ## go through sub folders
    cd "$i" || return
    ## in the git folder here
    pwd
    ## add files we care about
    find . -type f -not -path '*/\.git/*' -print0 |\
           xargs -t -0 git add -f
    ## commit and push
    git commit -uno -a -m "Commit $(date +'%F %R')"
    git push -f
    git push --tag 
done


echo "FIN"

exit 0
