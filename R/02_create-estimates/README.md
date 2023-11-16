# Create Local Health Department (LHD) demographic and socioeconomic status estimates
The R scripts in this directory can be used to generate updated demographic and socioeconomic estimates for local health departments (LHDs). NACCHO will run this code when one of the following conditions is met:

1.  A new American Community Survey (ACS) 5-year dataset is released
2.  An updated denominator file is available

New ACS 5-year datasets are released in December, and IPUMS NHGIS tries to add them within six weeks of their release. For example, the 2018-2022 ACS dataset will be released in December 2023. IPUMS NHGIS should have them available by the end of January 2024. Thus, LHD-level estimates based on the 2018-2022 dataset can be generated in January or February 2024.

## Overview of ACS-based workflow
The workflow to generate demographic and socioeconomic estimates based on ACS 5-year datasets follows these steps:

1. Update the `naccho_configuration.R` script with one or both of the following:
  - LHD vintage
  - ACS 5-year dataset
2. Run the `naccho_configuration.R` script
  - This creates variables and file paths used by subsequent scripts
3. Run the `acs_totals_workflow.R` script
  - This script creates a CSV with ACS-based totals (total population, total households, total housing units), counts by major race groups, and counts by housing tenure (owner-occupied, renter-occupied)
  - A CSV file with the name lhd{year}_acstotals.csv will be written to the `output-data/{acs_dataset}` directory
  - This script also creates data frames that are required for the scripts that start with **ipf**
4. Run the **ipf** scripts
  - The **ipf** scripts can be run in any order
  - Each **ipf** script creates a CSV where every row is an LHD and every column provides infomration about people or households residing in the LHD
  - The **ipf** scripts write out CSV files with the following names  
    - lhd{year}_grossrent.csv
    - lhd{year}_hhincome.csv
    - lhd{year}_ownoccvalue.csv
    - lhd{year}_sexage.csv

Refer to `output-data/estimates_codebook.xlsx` for variable definitions for each file.

## Overview of urban/rural workflow
The workflow to generate urban/rural counts based on the 2020 decennial census follows these steps:

1. Update the `naccho_configuration.R` script with following:
  - LHD vintage
2. Run the `naccho_configuration.R` script
  - This creates variables and file paths used by subsequent scripts
3. Run the `urban_rural_lhd_workflow.R` script
  - This script creates a CSV with urban/rural counts 
  - The CSV file will be written to the `output-data/urban_rural/` directory
  - The CSV file name will look like this:
    - lhd2022_urban_rural_2020.csv
      - lhd2022: this is the LHD vintage for which you are generating counts, and is based on the value of the `lhd_vintage` variable set in the naccho_configuration.R script
      - urban_rural_2022.csv: this is the fixed part of the file name

Refer to `output-data/estimates_codebook.xlsx` for variable definitions for each file.

## Scripts
Generating demographic and socioeconomic status estimates for LHDs is accomplished by running a set of R scripts in RStudio. This section lists those scripts and provides short descriptions of what they create.

1. `naccho_configuration.R`
    - This script must be run before any of the subsequent scripts
    - This script creates variables such as file paths, vintages, datasets, and tables
    - This file does not process any data; instead, it sets up the environment for subsequent processing
    - When a new ACS 5-year dataset is released, this script can be updated so new estimates can be created usign the new ACS dataset
    - There are two variables that must be set by the user - the ACS dataset and the LHD vintage. Instructions for setting these variables are in the script itself
2. `urban_rural_lhd_workflow.R`
    - This script uses 2020 decennial census data to generate counts and proportions of the urban and rural population in each LHD
    - The 2020 decennial census is the only census product that supports the calculation of urban and rural counts for LHDs
3. `acs_totals_workflows.R`
    - This script generates LHD-level estimates of the following variables from ACS 5-year data:
        - Total population
        - Population by race 
            - White alone
            - Black alone
            - American Indian or Alaska Native alone
            - Asian alone
            - Native Hawaiian or other Pacific Islander alone 
            - Some other race alone
            - Two or more races
        - Households
        - Housing units
        - Housing tenure (for occupied housing units)
            - Owner-occupied housing units
            - Renter-occupied housing units
4. `ipf_sex_sexage.R`
    - This script generates LHD-level estimates by sex and age for 23 age categories
5. `ipf_hhincome.R`
    - This script generates LHD-level estimates of households reporting an income that falls into a particular bin (e.g., \$75,000 - \$99,999)
6. `ipf_ownoccvalue.R`
    - This script generates LHD-level estimates of owner-occupied housing units whose value falls into a particular bin (e.g., \$250,000 - \$299,999)
7. `ipf_grossrent.R`
    - This script generates LHD-level estimates of renter-occupied housing units whose gross rent falls into a particular bin (e.g., \$1,000 - \$1,249)

The scripts prefixed with **ipf** attempt to use iterative proportional fitting (IPF) to generate LHD-level estimates. In some states, IPF will not run. In those cases, we revert to down-weighting to generate the LHD estimates. We flag LHDs whose estimates were successfully generated via IPF.  

## Questions
If you have questions about this workflow or the data please reach out to David Van Riper (vanriper@umn.edu).