#!/bin/env bash

#### Create a tag list from "tags:" array in the yaml of all md

echo ""
echo "List of tags"
echo "============"
grep -h "[ ]*tags:.*" ./**/*.md |\
    sed 's/tags:[ ]*\[//g' |\
    sed 's/\]//g'          |\
    tr " " "\n"            |\
    sed '/^$/d'            |\
    sort                   |\
    uniq -c

