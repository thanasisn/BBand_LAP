#!/bin/bash
## created on 2022-12-19

#### Create a master md file with h1 header from folders names

## this should be already sorted
## list of files from Makefile
args=( "$@" )

taglist="Tag_list.md"

## init document
targetfile="Readme.md"
echo "" > "$targetfile"

## add preamble
(
cat "./About.md"
echo
echo "-----"
echo
)>> "$targetfile"


args=( $(find -name "*.md" | grep "[0-9]\{4\}" | sort -V) )



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
        #TODO skip yaml header
        if [[ -n "$infile" ]]; then
            echo " - $infile"
            (
            cat "$infile" |\
                sed '1 { /^---/ { :a N; /\n---/! ba; d} }'
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
cat "./Instructions.md"
) >> "$targetfile"

(
echo ""
echo "List of tags"
echo "============"
grep "\[//\]" **/*.md |\
    cut -d"#" -f2- |\
    cut -d":" -f2- |\
    sed 's/)//g' |\
    tr "," "\n" |\
    sed -e 's/^[ ]\+//' -e 's|[ ]\+$||' |\
    sort |\
    uniq -c

echo ""
echo "New List of tags"
echo "============"
grep -h "[ ]*tags:.*" ./**/*.md |\
    sed 's/tags:[ ]*\[//g' |\
    sed 's/\]//g'          |\
    tr " " "\n"            |\
    sed '/^$/d'            |\
    sort                   |\
    uniq -c

) > "$taglist"

cat "$taglist" >> "$targetfile"

# Create TOC
"$HOME/PROGRAMS/gh-md-toc" --insert --no-backup "$targetfile"

exit 0
