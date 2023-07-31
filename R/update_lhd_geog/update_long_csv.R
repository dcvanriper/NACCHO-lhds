#######################################

## ---- update_long_csvs.R ----
#
# Author: Kate Knowles
# Date: 2023-07-30
#
# This script builds new long format csvs (county, cousub, and place) for NACCHO to update.

####################################### 

## ---- Load packages ----
library(tidyverse)
library(glue)
library(ipumsr)

tot_pop_table <- "B01001"
acs_dataset <- "2017_2021_ACS5a"
lhd_vintage <- "lhd2023"
former_lhd_vintage <- "lhd2022"

user <- "knowlesk"
dir_base <- glue("C:/Users/{user}")
dat_dir <- glue("{dir_base}/Box/NACCHO GIS/data_working")
geog_update_dir <- glue("{dat_dir}/tables/geog_update_data/{lhd_vintage}")
cty_update_dir <- glue("{geog_update_dir}/county")
cousub_update_dir <- glue("{geog_update_dir}/cty_sub")
place_update_dir <- glue("{geog_update_dir}/place")

## Get new county, cousub, and place codes from API

## Build directory for nhgis downloads

## Just for 2022...
if(!dir.exists(geog_update_dir)) {
  dir.create(geog_update_dir)
}

if(!dir.exists(cty_update_dir)) {
  
  dir.create(cty_update_dir)
  
}

if(!dir.exists(cousub_update_dir)) {
  
  dir.create(cousub_update_dir)
  
}

if(!dir.exists(place_update_dir)) {
  
  dir.create(place_update_dir)
  
}

# if the files don't already exist...

## Define extract
# County extract
cty_update_extract <- define_extract_nhgis(
  description = glue("County total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "county"
  )
)

# County Subdivision extract
cousub_update_extract <- define_extract_nhgis(
  description = glue("County subdivision total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "cty_sub"
  )
)

# Place extract
place_update_extract <- define_extract_nhgis(
  description = glue("Place total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "place"
  )
)

## Submit extracts and select and rename columns
# new_counties <- submit_extract(cty_update_extract) %>%  # Submit extract
#   wait_for_extract() %>%  # Wait until extract is ready
#   download_extract(cty_update_dir) %>%  # Download extract to directory created above
#   read_nhgis()  # Read county table into R session

new_counties <- read_nhgis(glue("{cty_update_dir}/nhgis0148_csv.zip"))

ctys_new <- new_counties %>%
  select(GISJOIN, 
         STATEA, 
         COUNTYA, 
         NAME_CTY = COUNTY)

# new_cousubs <- submit_extract(cousub_update_extract) %>%
#   wait_for_extract() %>%
#   download_extract(cousub_update_dir) %>%
#   read_nhgis()

new_cousubs <- read_nhgis(glue("{cousub_update_dir}/nhgis0149_csv.zip"))

cousubs_new <- new_cousubs %>%
  select(GISJOIN, 
         STATEA,
         COUNTYA, 
         COUSUB = COUSUBA, 
         NAME_CS = COUSUB) %>%
  filter(COUSUB != "00000" & COUSUB != "99999")

# new_places <- submit_extract(place_update_extract) %>%
#   wait_for_extract() %>%
#   download_extract(place_update_dir) %>%
#   read_nhgis()

new_places <- read_nhgis(glue("{place_update_dir}/nhgis0150_csv.zip"))

places_new <- new_places %>%
  select(GISJOIN, 
         STATEA,
         PLACE = PLACEA,
         NAME_PL = PLACE)

# Vector of state abbreviations
states <- state.abb[!state.abb == "RI"] # RI is not part of NACCHO so removed
states <- c(states, "DC") # add DC to the list

## Build directory for new output lists

## Read County csv in

## Function for new county lists by state
update_long_lists <- function(state_list)  {
  
  state <- state_list
  
  new_year_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}")
  last_year_st_dir <- glue("{dat_dir}/tables/lists_to_update/{former_lhd_vintage}/{state}")
  county_old_path <- glue("{last_year_st_dir}/{state}_county_lhds.csv")
  cousub_old_path <- glue("{last_year_st_dir}/{state}_cousub_lhds.csv")
  place_old_path <- glue("{last_year_st_dir}/{state}_place_lhds.csv")
  
  new_year_st_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/{state}")
  county_new_path <- glue("{new_year_st_dir}/{state}_county_lhds.csv")  
  cousub_new_path <- glue("{new_year_st_dir}/{state}_cousub_lhds.csv")
  place_new_path <- glue("{new_year_st_dir}/{state}_place_lhds.csv")

  # Check for state directory - create if it doesn't exist
  if (dir.exists(new_year_st_dir)) {
    
    print(glue("Folder already exists for {state}. Skipping {state}"))
  } else {
    dir.create(new_year_st_dir)
    print(glue("Folder created for {state}."))
  }
  
  ## Counties
  
  if (file.exists(county_old_path)) {

    cty_long <- read_csv(county_old_path, col_types = cols(.default = col_character())) %>%
      select(STATEA,
             COUNTYA,
             naccho_id,
             lhd_name)

    # Filter new county list to state
    cty_st_new <- ctys_new %>%
      filter(STATEA %in% cty_long$STATEA)

    cty_st_new_join <- cty_st_new %>%
      full_join(cty_long)

    # Convert NAs to blanks
    cty_st_new_join <- replace(cty_st_new_join, is.na(cty_st_new_join), " ")

    # Write CSV list
    write_csv(cty_st_new_join, county_new_path)

    print(glue("County CSV written for {state}"))

  } else {

    print(glue("No county shapefile for {state}"))

  }
  
  
  ## County Subdivision
  
  if (file.exists(cousub_old_path)) {
    
    cousub_long <- read_csv(cousub_old_path, col_types = cols(.default = col_character())) %>%
      select(STATEA, 
             COUNTYA, 
             COUSUB,
             naccho_id, 
             lhd_name)
    
    # Filter new county list to state
    cousub_st_new <- cousubs_new %>%
      filter(STATEA %in% cousub_long$STATEA)
    
    cousub_st_new_join <- cousub_st_new %>%
      full_join(cousub_long)
    
    # Convert NAs to blanks
    cousub_st_new_join <- replace(cousub_st_new_join, is.na(cousub_st_new_join), " ")
    
    # Write CSV list
    write_csv(cousub_st_new_join, cousub_new_path)
    
    print(glue("County Subdivision CSV written for {state}"))
    
  } else {
    
    print(glue("No county subdivision shapefile for {state}"))
    
  }
  
  
  ## Places
  
  if (file.exists(place_old_path)) {

    place_long <- read_csv(place_old_path, col_types = cols(.default = col_character())) %>%
      select(STATEA,
             PLACE,
             naccho_id,
             lhd_name)

    # Filter new county list to state
    place_st_new <- places_new %>%
      filter(STATEA %in% place_long$STATEA)

    place_st_new_join <- place_st_new %>%
      full_join(place_long)

    # Convert NAs to blanks
    place_st_new_join <- replace(place_st_new_join, is.na(place_st_new_join), " ")

    # Write CSV list
    write_csv(place_st_new_join, place_new_path)

    print(glue("Place CSV written for {state}"))

  } else {

    print(glue("No place shapefile for {state}, blank place CSV written"))

  }
  
}

## ---- Write out update lists ---- 

# Run function to create csvs for all (or a select list) of states
update_fips_lists <- map(states, ~update_long_lists(.x))



# If county csv exists from prior year...
# Read in csv

# filter US counties to that state

# Full join onto new county list

# Flag differences - LHDs lost and new codes

# Write out list of only matches

# Write out df of lost lhds?

## Repeat for Cousubs and places
