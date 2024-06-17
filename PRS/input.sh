#!/bin/bash

folder_path="./range/"

for file in "$folder_path"/*; do
    if [ -f "$file" ];  then
        filename=$(basename -- "$file")
        filename_noext="${filename%.*}"

        plink --bfile "geno_eur" --extract "$file" --range --make-bed --out "./PRS-input/geno__${filename_noext}"
    fi
done
