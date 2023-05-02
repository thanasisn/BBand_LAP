#!/bin/bash
## created on 2023-04-03

#### Build the Broad Band database from scratch.


info() { echo ; echo "$(date +%F_%T) :: $* " >&1; }
mkdir -p "$(dirname "$0")/LOGs/"
LOG_FILE="$(dirname "$0")/LOGs/$(basename "$0")_$(date +%F_%T).log"
ERR_FILE="$(dirname "$0")/LOGs/$(basename "$0")_$(date +%F_%T).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)
info "START :: $0 :: $* ::"


info "Get data from Sirena"
"$HOME/BBand_LAP/build_db/Get_data_from_sirena.sh"

info "Build the main database"
"$HOME/BBand_LAP/build_db/Build_BB_DB.R"

info "Create plots and reports"
"$HOME/BBand_LAP/inspect_db/Inspect_BB_DB.R"

info "Run other processes"
"$HOME/BBand_LAP/process/Process_BB_DB.R"

exit 0 
