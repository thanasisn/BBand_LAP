#!/bin/bash
## created on 2022-04-15

#### This is to build TSI data series for using with Broad-Band measurements

info() { echo ; echo "$(date +%F_%T) :: $* " >&1; }
mkdir -p "$(dirname "$0")/REPORTS/LOGs/"
LOG_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).log"
ERR_FILE="$(dirname "$0")/REPORTS/LOGs/$(basename "$0")_$(date +%F_%T).err"
exec  > >(tee -i "${LOG_FILE}")
exec 2> >(tee -i "${ERR_FILE}" >&2)
info "START :: $0 :: $* ::"


## Operational data preparation

info "Get TSI model from NOAA"
Rscript "$HOME/TSI/retreive_data/TSI_get_NOAA.R"

info "Get TSI from TSIS"
Rscript "$HOME/TSI/retreive_data/TSI_get_TSIS.R"

info "Prepare NOAA TSI for LAP"
# ## just run it 
# Rscript "$(dirname "$0")/TSI_NOAA_LAP.R"
## or build the report
R -e "rmarkdown::render(input       = \"~/TSI/TSI_NOAA_LAP.R\",
                        output_file = \"TSI_NOAA_LAP.pdf\",
                        output_dir  = \"~/TSI/REPORTS\")"

info "Prepare TSIS TSI for LAP"
# Rscript "$(dirname "$0")/TSI_TSIS_LAP.R"
R -e "rmarkdown::render(input       = \"~/TSI/TSI_TSIS_LAP.R\",
                        output_file = \"TSI_TSIS_LAP.pdf\",
                        output_dir  = \"~/TSI/REPORTS\")"

## Create a unified long term data series
info "Prepare TSI for use"
# Rscript "$(dirname "$0")/prepare_TSI_LAP.R"
R -e "rmarkdown::render(input       = \"~/TSI/prepare_TSI_LAP.R\",
                        output_file = \"prepare_TSI_LAP.pdf\",
                        output_dir  = \"~/TSI/REPORTS\")"

## end coding
printf "%s %-10s %-10s %-50s %f\n" "$(date +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$SECONDS"
exit 0
