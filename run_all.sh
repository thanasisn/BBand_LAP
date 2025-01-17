#!/bin/bash
## created on 2023-04-03

#### Run everything for this project

info() { echo ; echo "$(date +%F_%T) :: $* " >&1; }
mkdir -p "$(dirname "$0")/REPORTS/LOGs/"
LOG_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).log"
OUT_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).out"
ERR_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).err"
exec  > >(tee -i "${OUT_FILE}")
exec  > >(tee -i "${LOG_FILE}") 2>&1
exec 2> >(tee -i "${ERR_FILE}" >&2)
info "START :: $0 :: $* ::"


info "Run other processes"
"$HOME/BBand_LAP/process/Process_BB_DB.R"

info "Update Readme.md file"
"$HOME/BBand_LAP/.update_readme.sh"


exit 0
