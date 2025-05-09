#!/bin/bash

#### Start a batch run of LibRadTran

## Parallel files
SSH_LOGIN="/home/athan/BASH/PARAMS/parallel/sagan_blue_kostas"
SLURP_HOSTS="/home/athan/BASH/PARAMS/parallel/pssh_host_files"

## Project files
ARGS_LIST="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/run.list"
JOB_RESUM="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/resume.file"
HOST_OUTP="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/io_repo"
JOBS_THRO="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/job_throtle.par"
GATHER_DR="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/par_out"

THRO="120%"

echo " ************************************* "
echo " ** starting with $THRO of the cores!! ** "
echo " ************************************* "
echo "change file $JOBS_THRO to change throttling"


## set 50% of the cores as default
echo "$THRO" > "$JOBS_THRO"

## tic
SEC1=$(date +%s)

## remove resume-failed when starting a new job
##TODO try use an empty file
parallel                            \
    --jobs           "$JOBS_THRO"   \
    --progress                      \
    --eta                           \
    --resume-failed                 \
    --sshloginfile   "./hosts"      \
    --joblog         "$JOB_RESUM"   \
    --arg-file       "$ARGS_LIST"

    #--sshloginfile   "$SSH_LOGIN"   \
    # --results        "/dev/null"   \
# --timeout 1000%
# --jobs 1


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

# SEC3=$(date +%s)
# DIFFSEC=$((SEC3 - SEC2))
# echo "Took $(date +%H:%M:%S -ud @${DIFFSEC})"

exit 0
