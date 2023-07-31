# Instructions for Updating LHD Coverage Files

## Step 1: Install R packages
See Dave's documentation (R/create-estimates/README)

## Step 2: Edit and run configuration file (lhd_updates_config.R)
1. Set data directory
2. Set new lhd vintage - current year of update
3. Set old lhd vintage - last year denominator was updated
4. Set acs dataset that will be used to build the population estimates

## Step 3: Build lhd lists for new year
Run update_long_csv.R - This script creates a new folder with starting lists for the new year. These are the files you will edit that will then feed into the scripts to build the estimates.

## Step 4: Edit lists of LHDs
Manually edit the long CSV files created by update_long_csv.R to reflect new LHD coverage.   
There will be two files for each state, either a County and a Place file, or a County Subdivision (cousub) and a Place file.

## Step 5: Build new wide denominator file
Run build_wide_denominator.R - This script creates a new wide file with one record per NACCHO ID with all Counties, County Subdivisions, and Places covered by a single LHD. This file is only to be used as a refernce for NACCHO in the update process. Only the long format CSVs will be used to build the final updates.