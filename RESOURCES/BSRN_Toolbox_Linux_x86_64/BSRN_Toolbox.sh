#!/bin/sh
# BSRN_Toolbox.sh

#echo queries:
#set -e

# script to run the PANGAEA_Tool BSRN_Toolbox on a Linux 64 bit system

#echo "Start"

TOOLPATH=$PWD/bin
TOOLNAME=`basename -s .sh $0`

#echo ${TOOLPATH}
#echo ${TOOLNAME}

# set LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${TOOLPATH}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

#set PATH
export PATH=${PATH}:${TOOLPATH}

#run application
exec ${TOOLPATH}/${TOOLNAME}
