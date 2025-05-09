#!/bin/bash
# -------------------------------------------------------------------
# Copyright 2016-2024  Athanasios Natsis <natsisthanasis@gmail.com>
#
# run one uvspec on one core of some host
# with proper arguments in proper environment, hopefully!
#
# -------------------------------------------------------------------

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    exit
fi

# jobid="$(echo "$1" | cut -d' ' -f1)"
# options="$(echo "$1" | cut -d' ' -f2)"

# libpath=$1   ## Libradtran file path .../libRadtran-2.0/
# workdir=$2   ## Libradtran output folder for this model family
jobid=$1     ## unique id for this run
options=$2   ## serialized string  with uvspec options
             ##    "atmosphere_file@@aattmmoo=afglms.dat@source@@solar@@ssoollaa=kurudz_1.0nm.dat@mol_modify@@O3@@290@@DU@albedo@@0.05@sza@@26@altitude@@0.062694168@rte_solver@@sdisort@number_of_streams@@6@wavelength@@250@@5025@pseudospherical@quiet@"

UVSPEC="$HOME/LibRadTranG/libRadtran-2.0.5/bin/uvspec"
WPTEMP="$HOME/LibRadTranG/Clear_sky_model_AERONET_monthly/io_repo"
DATA="$HOME/LibRadTranG/libRadtran-2.0.5/data"

## check executable location
if [ ! -f "$UVSPEC" ]; then
    echo "Can not find uvspec"
    exit 3
fi
## check data folder location
if [ ! -d "$DATA" ]; then
    echo "Can not find data folder $DATA"
    exit 4
fi


## have jobid?
if [ -z "$jobid" ]; then
    echo "Empty variable 'jobid'"
    exit 7
fi

## have libratran options?
if [ -z "$options" ]; then
    echo "Empty variable 'options'"
    exit 8
fi

## create working folder
mkdir -p "${WPTEMP}"
## files for this job
INPUT="${WPTEMP}/LBT_${jobid}.inp"
OUPUT="${WPTEMP}/LBT_${jobid}.out"
ERPUT="${WPTEMP}/LBT_${jobid}.err"


## create input file
(
echo $options | sed 's/@@/ /g' | sed 's/@/\n/g' | while read line; do
    echo $line |\
        sed "s@aattmmoo=@"$DATA/atmmod/"@g"     |\
        sed "s@ssoollaa=@"$DATA/solar_flux/"@g"
done
) > "$INPUT"

#### ready to run uvspec
tic=$(date +"%s")
loa="$(uptime | grep -o "load .*")"

## set env variable
export LIBRADTRAN_DATA_FILES=${DATA}
## run libradtran
( $UVSPEC < "$INPUT" > "$OUPUT" ) >& "$ERPUT"

wait
wait
gzip -f "$OUPUT"
tac=$(date +"%s")


## helpful info for this run and uvspec error collector
(
    echo "$loa"
    uptime | grep -o "load .*"
    echo "hostname=$(hostname)"
    date +"%F %T"
    echo "$tic"
    echo "$tac"
) >> "$ERPUT"


exit 0
