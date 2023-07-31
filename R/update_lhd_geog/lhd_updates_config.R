#######################################

## ---- lhd_updates_config.R ----
#
# Author: Kate Knowles
# Date: 2023-07-27
#
# This script sets the constants used in all scripts used in the process to update the lists of LHDs.

####################################### 

library(tidyverse)
library(glue)
library(dplyr)
library(ipumsr)

# Set base working data directory
dat_dir <- glue("C:/Users/knowlesk/Box/NACCHO GIS/data_working")

# Set lhd year
# lhd_vintage <- "lhd2022"
lhd_year <- "2022"