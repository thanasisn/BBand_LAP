#!/bin/bash
## created on 2023-04-03

#### Build the Broad Band database from scratch.

echo ""
echo "ARE YOU SURE?"
echo ""

read -p "Do you want to NUKE the database and rebuild everything? " conf

if   [[ $conf == "yes" ]]; then
    echo "Start form scratch"
else
    echo "Exit"
    exit 99
fi

rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB"
rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB_metadata.parquet"
rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB.stopfile"

"$HOME/BBand_LAP/build_db/Get_data_from_sirena.sh"

"$HOME/BBand_LAP/build_db/Build_BB_DB.R"

"$HOME/BBand_LAP/inspect_db/Inspect_BB_DB.R"

"$HOME/BBand_LAP/process/Process_BB_DB.R"

exit 0 
