#!/bin/bash
## created on 2022-12-19

#### Create a master md file with h1 header from folders names

## this should be already sorted
## list of files from Makefile
args=( "$@" )

## init document
targetfile=".markdowncontent"
echo "" > "$targetfile"

## add preamble
(
cat "./About.md"
echo 
echo "-----"
echo
)>> "$targetfile"


## get all the years from files
years=($(for af in "${args[@]}"; do echo "$(basename "$(dirname "$af")")"; done | sort -u | grep "[0-9]*" ))

## loop years
for ay in "${years[@]}"; do
    echo
    echo "** $ay **"
    ( 
    echo
    echo "\newpage"
    echo
    echo "# $ay"
    echo
    # .justified
    # .ragged 
    echo '::: {.columns columngap=4em column-rule="1px solid black"}'
    # echo ":::columns"
    echo
    ) >> "$targetfile"
    
    ## loop all files of year
    for af in "${args[@]}"; do
        infile="$(echo "$af" | grep "/$ay/")"
        ## append the appropriate files only
        if [[ -n "$infile" ]]; then
            echo " - $infile"
            (
            cat "$infile" 
            echo ""       
            # echo "------" 
            echo ""        
            ) >> "$targetfile"
        fi
    done
    (
    echo
    echo ":::"
    echo
    ) >> "$targetfile"
done 

## add tail
(
echo
# echo "\newpage"
echo "-------"
echo
cat "./Readme.md" 
) >> "$targetfile"

exit 0 
