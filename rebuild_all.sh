#!/bin/bash
## created on 2023-04-03

#### enter description here

rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB"
rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB_metadata.parquet"
rm -rfv "$HOME/DATA/Broad_Band/Broad_Band_DB.stopfile"

"$HOME/BBand_LAP/build_db/Get_data_from_sirena.sh"
"$HOME/BBand_LAP/build_db/Build_BB_DB.R"
"$HOME/BBand_LAP/inspect_db/Inspect_BB_DB.R"



## end coding
exit 0 
