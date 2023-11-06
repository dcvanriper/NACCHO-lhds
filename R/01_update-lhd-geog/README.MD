# Instructions for Updating LHD Coverage Files

The first step in updating the population estimates for local health departments (LHDs) is to create updated files that represent the current coverage areas of all LHDs. This process involves the following steps:

1. Create new long format CSVs for updating
2. Update long format CSVs manually
3. Create new wide format denominator files for NACCHO reference - these are not used to create population estimates

To get the most up to date population estimates, you will need to use the newest geographic units. Each year, the updated data are released from NHGIS in December (is this right?). You will use those data to make the new files for that year to update. For example, in 2023 you will need to wait until early December to start this process with the newly released 2022 ACS data to get the most up to date population estimates for the new LHD boundaries.

## Step 1: Initial Set-up
Follow the instructions found in the README.md file on the homepage of the git repository (https://github.com/dcvanriper/NACCHO-lhds/tree/main). Once the necessary software and directories are setup, proceed with the following steps to update the LHD geographic coverage files.

## Step 2: Edit and run configuration file
To run the R scripts used in this process, some basic parameters need to be set for each update year. Follow the instructions in `lhd_updates_config.R` to:

1. Base data directory used by NACCHO staff 
2. LHD vintage - current year of update
3. Former LHD vintage - last year denominator was updated
4. ACS dataset that will be used later to build the population estimates    

Once this script is edited, run the entire script. Anytime NACCHO staff clears their R environment, or runs the scripts in a new session, they will need to re-run this configuration script. 

## Step 3: Build LHD coverage CSV files for new year
Run `build_new_lhd_files.R` to create a new folder with starting lists for the new year. These are the files you will edit that will then feed into the scripts to build the estimates. This script accomplishes four things:

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

## Step 4: Edit LHD coverage CSV files
Manually edit the long CSV files created by running `build_new_lhd_files.R` to reflect new LHD coverage. There will be two files for each state, either a County and a Place file, or a County Subdivision (cousub) and a Place file. All files created in Step 3 will be useful in the update process, but only the long format csvs need to be updated to calculate the population estimates.    
#### Files to edit (long format):
1. {state}_county_lhds.csv
2. {state}_cousub_lhds.csv
3. {state}_place_lhds.csv
#### Files to reference as needed:
1. dropped_cty_records.csv
2. dropped_cousub_records.csv
3. dropped_place_records.csv 
4. denominator_{former_lhd_vintage}.csv
5. fips_denominator_{former_lhd_vintage}.csv
6. lhd_geog_updates_codebook.xlsx (This file details the contents of each CSV listed above)

The goal of this process is to create accurate long format CSVs that reflect the current coverage areas of all member LHDs. This is accomplished by adding or removing the long format CSVs  
### Editing guide
#### 1. Review long format CSVs
- Identify changes that need to be made based on outreach process.
#### 2. Edit long files
- Add and remove naccho ids as necessary to reflect additions of new LHDs, loss of LHDs, and changing coverage areas of LHDs.
#### 3. Reconcile dropped records
- Go through all dropped record files and determine whether the naccho id attached to each record needs to be added to a different record in the long format CSVs, the LHD was eliminated if it only covers geographic units in the dropped records list, or it remains, only serving geographic units that were not dropped.
#### 4. Check new records
- Check records flaged as new ("1" in the new_record column) to determine if a NACCHO ID should be attached to them.

*Things to note as NACCHO staff make updates:*
1. LHDs may cover one or more geographic units in a single file (ex. one or more Counties in {state}_county_lhds.csv) or may be cover units in multiple files (ex. one or more County Subdivisions in {state}_cousub_lhds.csv and one or more Places in {state}_place_lhds.csv).
2. The population estimate sctripts will handle any LHD coverage overlaps by always defaulting to the Place (city or town) LHD. For example if a Place health department exists, and the County the Place is in also has a health department, the population estimates for the Place health department will be for the entire Place, and the county health department estimates will be for the remainder of the County that does not overlap with the Place.
3. LHDs listed in the dropped_{Census unit}_records.csv files should be inspected to determine if they need to be reasigned to different records in the long files.
4. All files must remain in CSV format. The scripts to create estimates and build the files to update the geographic coverage for the next year are built to handle CSV files and will not run if the files are saved as .xlsx or any other non-CSV format. 

## Step 5: Rerun configuration file
Run `lhd_updates_config.R` again to set the pparameters used to build the new wide denominator file. This step should not require editing, however, please double check the script to ensure the below items match the version of `lhd_updates_config.R` that was run in **Step 2**:

1. Base data directory used by NACCHO staff 
2. LHD vintage - current year of update
3. Former LHD vintage - last year denominator was updated
4. ACS dataset that will be used later to build the population estimates    

Once this script is edited, run the entire script. Anytime NACCHO staff clears their R environment, or runs the scripts in a new session, they will need to re-run this configuration script. 

## Step 6: Build new wide denominator files
Run `build_wide_denominator.R` - This script creates two new wide denominator files with one record per NACCHO ID with all Counties, County Subdivisions, and Places covered by a single LHD. One file (denominator_{lhd_vintage}.csv) includes only the names of geographic units covered by each LHD, and the second file (fips_denominator_{lhd_vintage}.csv) includes FIPS codes for each geographic unit. These files are only to be used as a refernce for NACCHO in the update process. Only the long format CSVs will be used to build the final updates.

## Step 7: Final review
Review new denominator files (either one) to ensure they reflect the expected coverage. If there are any errors identified, return to the long format CSV files found in the state directories and make the necessary changes there. Then repeat **Step 6**. This will overwrite the two denominators files which you can review for a second time. If you want to keep a version of the incorrect files you will need to rename them and/or move them to a different location to avoid them being overwritten.

Once the final review is complete, the LHD geographic coverage update process is complete and the files are ready to be used in the population estimates update