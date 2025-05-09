#!/bin/bash

mcomand="$1"

#RES="$(parallel-ssh -i -h "$HOME/.pssh_host_files" ps u -C R)"
RES="$(parallel-ssh -i -h "$HOME/.pssh_host_files" ps u -C uvspec )"

title="$(echo "$RES" | grep "TIME" | head -n 1 | sed 's/COMMAND//g' | sed 's/USER//g')"

echo ""
echo "       $title"

#echo "$RES" | grep SUCCESS

paste <( echo "$RES" | grep "\[SUCCESS\]" | cut -d' ' -f4 )                         \
      <( echo "$RES" | grep "uvspec"  | sed 's/\/usr.*//g' | sed 's/athan//g' | sed 's/\/home\/condor\/executables\/libRadtran-2.0-64\/bin\///g' ) | sort -n -k 10

