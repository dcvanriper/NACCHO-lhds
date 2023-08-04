#######################################

## ---- lhd_updates_config.R ----
#
# Author: Kate Knowles
# Date: 2023-07-27
#
# This script sets the constants used in all scripts necessary to update the lists of LHDs.

####################################### 

# Set base working data directory
# dat_dir <- "C:/Users/knowlesk/Box/NACCHO GIS/data_working"
# dat_dir <- "E:/contract/naccho-gis/lhd-updates/data"
dat_dir <- "/Documents/Work/NACHO/lhds_to_update"

# Set lhd year
lhd_vintage <- "lhd2010"
former_lhd_vintage <- "lhd2022"

## Set data parameters - these should match naccho_configuration.R

# The dataset that will be used in the second part of the workflow to build the estimates.
# This serves as a base for all of the current County, County Subdivision, and Place codes for the update year.
acs_dataset <- "2006_2010_ACS5a"

# The total population table from the above dataset.
# This may or may not need to be changed for a given year.
tot_pop_table <- "B01003"

