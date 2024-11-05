#!/bin/bash
# "http://weather.uwyo.edu/cgi-bin/sounding?TYPE=TEXT%3ALIST&YEAR=2016&MONTH=02&FROM=all&STNM=LGTS"
# "http://weather.uwyo.edu/cgi-bin/sounding?region=europe&TYPE=TEXT%3ALIST&YEAR=2016&MONTH=02&FROM=all&STNM=LGTS"
# "http://weather.uwyo.edu/cgi-bin/sounding?region=europe&TYPE=TEXT%3ALIST&YEAR=2016&MONTH=02&FROM=all&TO=2412&STNM=LGTS"

TIC=$(date +"%s")

## folder for initial data
folder="$HOME/DATA_RAW/uwyo/LGTS/"

## start from yesterday
d=$(date -d"yesterday" +"%F")

## and go back until
pastday="2022-01-01"

## remove this months file so will get it again at each run
rm "${folder}LGTS_$(date +"%Y-%m").txt.xz"

## loop all months
while [ "$(date -d "$d" +%Y%m%d)" -gt "$(date -d "$pastday" +%Y%m%d)" ]; do
    yy="$(date -d "$d" +%Y)"
    mm="$(date -d "$d" +%m)"
    url="http://weather.uwyo.edu/cgi-bin/sounding?TYPE=TEXT%3ALIST&YEAR=${yy}&MONTH=${mm}&FROM=all&STNM=LGTS"
    filename="${folder}LGTS_${yy}-${mm}.txt"

    ## skip on overwrite
    if [ ! -f "${filename}.xz" ]; then
        echo "$yy $mm   TRY GET IT"
        ## file don't exist here try to get it
        lynx -width=1000 -dump "$url" |  xz  > "${filename}.xz"
    else
        ## we have the file
        echo "$yy $mm               you have this"
    fi
    ## date counter increment
    d=$(date -I -d "$d - 1 month")
done

echo
echo "$0 finished"
echo
## end coding
TAC=$(date +"%s"); dura="$( echo "scale=6; ($TAC-$TIC)/60" | bc)"
printf "%s %-10s %-10s %-50s %f\n" "$(date -u +"%F %H:%M:%S")" "$HOSTNAME" "$USER" "$(basename $0)" "$dura"
exit 0
