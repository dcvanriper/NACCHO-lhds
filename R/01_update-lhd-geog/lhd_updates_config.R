#######################################

## ---- lhd_updates_config.R ----
#
# Author: Kate Knowles
# Date: 2023-07-27
#
# This script sets the constants used in all scripts necessary to update the lists of LHDs

####################################### 

# Load necessary packages
library(glue)

## ---- Set directory ----

# Set Username 
user <- "knowlesk"

# Set base data directory -- if the username is the only change this does not need editing after the initial run
dat_dir <- glue("C:/Users/{user}/Box/NACCHO GIS/lhd-updates/data")

## ---- Set lhd year ----

# Current year - This is the year of the update, it does not need to match the ACS dataset
lhd_vintage <- "lhd2023"

# Year of last update - This is the year of the last update
former_lhd_vintage <- "lhd2022"


## ---- Set data parameters - these should match naccho_configuration.R ----

# The dataset that will be used in the second part of the workflow to build the estimates
## New ACS datasets will have a similar name to the one included below. E.g.:
## 2018_2022_ACS5a
## 2019_2023_ACS5a
## 2020_2024_ACS5a
acs_dataset <- "2017_2021_ACS5a"

# The total population table from the above dataset
# This does not need to be changed unless the Census Bureau changes their table names - unless the scripts fail do not edit this
tot_pop_table <- "B01003"

