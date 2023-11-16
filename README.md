# NACCHO-lhds

This repository contains scripts and documentation to (1) create updated long files for local health departments (LHDs) geographic coverage and (2) create updated population estimates for LHDs. NACCHO staff will want to update these data when:

1.  A new American Community Survey (ACS) 5-year dataset is released
2.  Updated coverage areas are available for LHDs

## This README.md

This particular README file provides instructions for installing required software, creating an account on IPUMS NHGIS, generating an API key to IPUMS NHGIS, and installing that API key in RStudio.

After all the steps in this README have been finished, you can then navigate to the `01_update-lhd-geog` or `02_create-estimates` directory to view the README related to those tasks.

## Initial Set-up

In order to run the R code, NACCHO staff need to install software, register for an account on IPUMS NHGIS, and obtain an Application Programming Interface (API) key for IPUMS NHGIS.

### Required Software

NACCHO staff need to install the following pieces of software:

1.  [R](https://cran.r-project.org/)

    -   The current version of R (as of 2023-07-29) is R 4.3.1 Beagle Scouts

2.  [RStudio](https://posit.co/download/rstudio-desktop/)

    -   The current version of RStudio (as of 2023-07-29) is 2023.06.1+524

Staff will need to choose the appropriate installer for the operating system they use (e.g., Windows, MacOS, Linux).

After installing R and RStudio, NACCHO staff should install the following R packages:

1.  ipumsr
2.  tidyverse
3.  glue
4.  vroom
5.  sf

To install these packages, staff should run the following commands in RStudio:

    install.packages("ipumsr")
    install.packages("tidyverse")
    install.packages("glue")
    install.packages("vroom")
    install.packages("sf")

R packages provide access to handy functions. Our code uses functions from the packages listed above. 

NACCHO staff will also need access to **Microsoft Excel** to edit spreadsheets that map LHDs onto census units (e.g., counties, places, or county subdivisions).

### Registering for an IPUMS NHGIS account

NACCHO staff will need to register for an account on [IPUMS NHGIS](https://www.nhgis.org/). NHGIS provides access, free of charge, to summary census and survey data, which is used to generate demographic and socioeconomic status estimates for LHDs.

To register for a free account, please:

1.  Visit the [NHGIS registration website](https://uma.pop.umn.edu/nhgis/user/new)
2.  Fill out all the required information
    -   If you can't find NACCHO in the list of Institutions or Employers, please click the SUGGEST A NEW INSTITUTION button after typing in NACCHO to the text box
3.  Click the SUBMIT button to submit your account request
4.  Our user management system will send a verification email to the provided email address. Please click the URL in the email to activiate the account.

### Obtaining a key for the NHGIS API

In order to use the NHGIS API, you need to get and use an API key. To access your API key, please:

1.  Visit the [IPUMS User Management System](https://uma.pop.umn.edu/)
2.  Click the LOGIN URL
3.  Sign in
4.  Click the View/create API key URL (this link will be near the bottom of the webpage)
5.  Copy and paste the API key into a text file (this API key will be used in a subsequent step)
    -   The API key is a 56-character string consisting of numerals, upper-case, and lower-case letters

### Setting up an IPUMS NHGIS API key in R

By default, ipumsr API functions assume that your key is stored in the IPUMS_API_KEY environment variable. You can also provide your key directly to these functions, but storing it in an environment variable saves you some typing and helps prevent you from inadvertently sharing your key with others (for instance, on GitHub).

You can save your API key to the IPUMS_API_KEY environment variable with set_ipums_api_key(). To save your key for use in future sessions, set save = TRUE. This will add your API key to your .Renviron file in your user home directory.

    # Load the ipumsr package into your R session
    require(ipumsr)

    # Save key in .Renviron for use across sessions
    set_ipums_api_key("paste-your-key-here", save = TRUE)

### Setting up the directory structure

We have written the code to create directories if they do not exist, but there are some directories that must be set up in advance of running any code. NACCHO staff will need to ensure their directory structure and names match the structure shown below. Directories and files marked with a star will be created automatically. The original .zip file shared with NACCHO in November, 2023 includes this structure and all 2022 LHD coverage lists and population estimates. It can be shared with multiple NACCHO staff members. If the directory is being built from scratch, you will need to re-create the structure below with matching file names for the scripts to run correctly.

    lhd-updates
       |
       - data
          |
          - lists_to_update
            |
            - {lhd_update_year}*
            - {lhd_update_year}*
          - tables
            |
              - geocorr
              - nhgis-csv
              - urban_rural_2020
          - output-data
              |
              - {acs_dataset}*
              - {acs_dataset}*
              - urban_rural*
      - R
        |
        - 01_update-lhd-geog
            |
            - individual R scripts
        - 02_create-estimates
              |
              - individual R scripts

Please refer to the README files in `01_update-lhd-geog` or `02_create-estimates` for more detailed documentation on the contents of these folders.

## Population estimate update timing
New ACS 5-year datasets are released in December, and IPUMS NHGIS tries to add them within six weeks of their release. For example, the 2018-2022 ACS dataset will be released in December 2023. IPUMS NHGIS should have them available by the end of January 2024. Thus, the lists of geographic units to update LHD coverage and subsequent population estimates based on the 2018-2022 dataset can be generated in January or February 2024.

## Contact Information
If you have questions regarding this workflow or the data please reach out to David Van Riper (vanriper@umn.edu) or Kate Knowles (knowlesk@umn.edu).