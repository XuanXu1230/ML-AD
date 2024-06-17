#!/bin/bash

# Define the directory path containing the .bed files
folder_path="./PRS-input/"

# Loop through all .bed files in the specified directory.
for file in "$folder_path"/*.bed; do
     # Check if the file exists and is a regular file.
    if [ -f "$file" ]; then
        # Extract the base name of the file without the path.
        filename=$(basename -- "$file")
        # Remove the file extension to get the base name for output.
        filename_noext="${filename%.*}"
        
        # Call the R script with various arguments for PRSice analysis.
        Rscript "./PRS/PRSice.R"\ 
            --prsice "./PRS/PRSice_linux" \
            --base "gwas_all.txt" --or --thread 5 \
            --target "./PRS-input/${filename_noext}" \
            --pheno "./prsice_input/phenotype.txt" \
            --pheno-col 'AD' \
            --stat OR \
            --binary-target T \
            --clump-r2 0.2 \
            --clump-kb 1000kb \
            --bar-levels 5e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1 \
            --quantile 20 \
            --missing SET_ZERO \
            --out "./PRS-out/${filename_noext}" \
            --all-score \
            --lower 5e-08 \
            --interval 5e-03 \
            --print-snp \
            --extract "PRS.valid"
    fi
done