#!/bin/bash

#### Start a batch run of LibRadTran

## Parallel files

## Project files
ARGS_LIST="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/run.list"


## tic
SEC1=$(date +%s)

# cat "$ARGS_LIST" | xargs -i -t -P 1 echo "{}"
cat "$ARGS_LIST" | xargs -i -t -P 1 sh -c "{}"

## tac
SEC2=$(date +%s)
DIFFSEC="$((SEC2 - SEC1))"
echo "Took $(date +%H:%M:%S -ud @${DIFFSEC})"


# ## get files from host to master
# parallel-slurp -r             \
#     -h    "$SLURP_HOSTS"      \
#     -L    "$GATHER_DR"        \
#           "$HOST_OUTP" "./"
#
# ## move data
# # parallel-rsync -h ~/.pssh_host_files -ra /home/athan/Improved_Aerosols_O3/DATA/ /home/athan/Improved_Aerosols_O3/DATA/GET

SEC3=$(date +%s)
DIFFSEC=$((SEC3 - SEC2))
echo "Took $(date +%H:%M:%S -ud @${DIFFSEC})"

exit 0
