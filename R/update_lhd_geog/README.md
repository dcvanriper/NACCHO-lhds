# Instructions for Updating LHD Coverage Files

The first step in updating the population estimates for local health departments (LHDs) is to create updated files that represent the current coverage areas of all LHDs. This process involves the following steps:

1. Create new long format CSVs for updating
2. Update long format CSVs manually
3. Create new wide format denominator files for NACCHO reference - these are not used to create population estimates.

## Initial Set-up
Follow the instructions found in the README.md file on the homepage of the git repository. Once the necessary software and directories are setup, proceed with the following steps to update the LHD geographic coverage files.

## Step 2: Edit and run configuration file (lhd_updates_config.R)
To run the R scripts used in this process, some basic parameters need to be set for each update year. Follow the instructions in lhd_updates_config.R to:

1. Set the local data directory used by NACCHO staff 
2. Set new lhd vintage - current year of update
3. Set old lhd vintage - last year denominator was updated
4. Set acs dataset that will be used later to build the population estimates

Once this script is edited, run the entire script. Anytime NACCHO staff clears their R environment, or runs the scripts in a new session, they will need to re-run this configuration script. 

## Step 3: Run script to build lhd lists for new year to edit (build_new_lhd_files.R)
Run build_new_lhd_files.R to create a new folder with starting lists for the new year. These are the files you will edit that will then feed into the scripts to build the estimates. This script accomplishes four things:

1. Creates a directory for the new update year and sub directories for each state.
2. Builds new long format CSV files to update. It creates these files by state and writes them out to state specific folders containing two files, one place list, and either a county or county subdivision list.    
    a. {state}_county_lhds.csv
    b. {state}_cousub_lhds.csv
    c. {state}_place_lhds.csv
3. Copies denominator files from the year prior for NACCHO reference.  
    a. denominator_{former_lhd_vintage}.csv
    b. fips_denominator{former_lhd_vintage}.csv
4. Builds lists of records in the long files from the prior year that are not in the long files for the new year. These records are dropped due to changes in the FIPS codes. These files should be referenced to ensure LHDs are attached to.  
    a. dropped_cty_records.csv
    b. dropped_cousub_records.csv
    c. dropped_place_records.csv

## Step 4: Edit lists of LHDs
Manually edit the long CSV files created by update_long_csv.R to reflect new LHD coverage. There will be two files for each state, either a County and a Place file, or a County Subdivision (cousub) and a Place file. All files created in Step 3 will be useful in the update process, but only the long format csvs need to be updated to calculate the population estimates. 

Things to note as NACCHO staff make updates:
1. LHDs may cover one or more geographic units in a single file (ex. {state}_county_lhds.csv) or may be attached to units in multiple files (ex. {state}_cousub_lhds.csv and {state}_place_lhds.csv).
2. The population estimate sctripts will handle any LHD coverage overlaps by always defaulting to the Place LHD. For example if a city health department exists, and the surrounding county also has a health department, the population estimates for the city health department will be for the entire city, and the county health department estimates will be for the remainder of the county that does not overlap with the city.
3. LHDs listed in the dropped_{Census unit}_records.csv files should be inspected to determine if they need to be reasigned to different records in the long files.

## Step 5: Build new wide denominator file
Run build_wide_denominator.R - This script creates a new wide file with one record per NACCHO ID with all Counties, County Subdivisions, and Places covered by a single LHD. This file is only to be used as a refernce for NACCHO in the update process. Only the long format CSVs will be used to build the final updates.

1. denominator_{lhd_year}