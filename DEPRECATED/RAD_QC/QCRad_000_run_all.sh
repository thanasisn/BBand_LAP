#!/bin/bash
## created on 2022-03-29

#### This is for daily usage.
## For building reports there is an R script


info() { echo ; echo "$(date +%F_%T) :: $* " >&1; }
mkdir -p "$(dirname "$0")/LOGs/"
LOG_FILE="$(dirname "$0")/LOGs/$(basename "$0")_$(date +%F_%T).log"
ERR_FILE="$(dirname "$0")/LOGs/$(basename "$0")_$(date +%F_%T).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)
info "START :: $0 :: $* ::"


info "LongShi Identification"
Rscript "$HOME/RAD_QC/QCRad_LongShi_v8_id_CM21_CHP1.R"


info "LongShi Application"
Rscript "$HOME/RAD_QC/QCRad_LongShi_v8_apply_CM21_CHP1.R"



## end coding
printf "%s %-10s %-10s %-50s %f\n" "$(date +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$SECONDS"
exit 0
