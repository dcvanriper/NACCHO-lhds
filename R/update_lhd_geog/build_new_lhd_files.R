#######################################

## ---- update_long_csv.R ----
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

## ---- Get data from API ----

## Define extracts

# County extract definition
cty_update_extract <- define_extract_nhgis(
  description = glue("County total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "county"
  )
)

# County Subdivision extract definition
cousub_update_extract <- define_extract_nhgis(
  description = glue("County subdivision total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "cty_sub"
  )
)

# Place extract definition
place_update_extract <- define_extract_nhgis(
  description = glue("Place total pop for {acs_dataset}"),
  datasets = ds_spec(
    acs_dataset,
    data_tables = tot_pop_table,
    geog_levels = "place"
  )
)

## Set temporary directories for API downloads
cty_temp_dir <- tempdir("county")
cousub_temp_dir <- tempdir("cousub")
place_temp_dir <- tempdir("place")

# cty_temp_dir <- glue("{dat_dir}/tables/lists_to_update/nhgis_api_calls/county")
# cousub_temp_dir <- glue("{dat_dir}/tables/lists_to_update/nhgis_api_calls/cousub")
# place_temp_dir <- glue("{dat_dir}/tables/lists_to_update/nhgis_api_calls/place")

# cty_test_dir <- glue("{dat_dir}/tables/api_dat_testing")

## Submit extracts and select and rename columns

## Counties
new_counties <- submit_extract(cty_update_extract) %>%  # Submit extract
  wait_for_extract() %>%  # Wait until extract is ready
  download_extract(cty_temp_dir) %>%  # Download extract to directory created above
  read_nhgis()  # Read county table into R session

# Select attributes
ctys_new <- new_counties %>%
  select(GISJOIN, 
         STATEA, 
         COUNTYA, 
         NAME_CTY_U = COUNTY) %>%
  mutate(current_year = "x")

ctys_new <- ctys_new %>% filter(row_number() %% 3 != 1)

## County subdivisions
new_cousubs <- submit_extract(cousub_update_extract) %>%
  wait_for_extract() %>%
  download_extract(cousub_temp_dir) %>%
  read_nhgis()

# Select attributes and filter
cousubs_new <- new_cousubs %>%
  select(GISJOIN, 
         STATEA,
         COUNTYA, 
         COUSUB = COUSUBA, 
         NAME_CS_U = COUSUB) %>%
  filter(COUSUB != "00000" & COUSUB != "99999") %>% # Eliminate records with no county subdivision (these are usually slivers around coasts and are records we do not need to include)
mutate(current_year = "x")

cousubs_new <- cousubs_new %>% filter(row_number() %% 4 != 1)

## Places
new_places <- submit_extract(place_update_extract) %>%
  wait_for_extract() %>%
  download_extract(place_temp_dir) %>%
  read_nhgis()

# Select attributes
places_new <- new_places %>%
  select(GISJOIN, 
         STATEA,
         PLACE = PLACEA,
         NAME_PL_U = PLACE) %>%
  mutate(current_year = "x")

places_new <- places_new %>% filter(row_number() %% 2 != 0)

## ---- Setup to build update lists ----
# Vector of state abbreviations
states <- state.abb[!state.abb == "RI"] # RI is not part of NACCHO so removed
states <- c(states, "DC") # add DC to the list

## Build directory for new output lists

# Set update directory
old_list_dir <- glue("{dat_dir}/tables/lists_to_update/{former_lhd_vintage}")
new_list_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}")

# Check for new lhd year directory and build if it does not exist
if (dir.exists(new_list_dir)) {
  print(glue("{lhd_vintage} update directory already exists"))
} else {
  dir.create(new_list_dir)
}

## ---- Function to update long CSVs ----

# Function to write out updated long format CSV lists for updating
update_long_lists <- function(state_list)  {
  
  state <- state_list
  
  last_year_st_dir <- glue("{dat_dir}/tables/lists_to_update/{former_lhd_vintage}/{state}")
  new_year_st_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/{state}")
  
  # Paths to state folders for old vintage
  county_old_path <- glue("{last_year_st_dir}/{state}_county_lhds.csv")
  cousub_old_path <- glue("{last_year_st_dir}/{state}_cousub_lhds.csv")
  place_old_path <- glue("{last_year_st_dir}/{state}_place_lhds.csv")
  
  # Paths to state folders for new vintage
  county_new_path <- glue("{new_year_st_dir}/{state}_county_lhds.csv")  
  cousub_new_path <- glue("{new_year_st_dir}/{state}_cousub_lhds.csv")
  place_new_path <- glue("{new_year_st_dir}/{state}_place_lhds.csv")
  
  # Check for state directory - create if it doesn't exist
  if (dir.exists(new_year_st_dir)) {
    
    print(glue("Folder already exists for {state}."))
  } else {
    dir.create(new_year_st_dir)
    print(glue("Folder created for {state}."))
  }
  
  ## Counties
  
  if (file.exists(county_old_path)) {
    
    # Load county file from prior year
    cty_long <- read_csv(county_old_path, col_types = cols(.default = col_character()))

    # Filter new county list to state
    cty_st_new <- ctys_new %>%
      filter(STATEA %in% cty_long$STATEA)
    
    # Join prior year naccho ids to current FIPS codes
    cty_st_new_join <- cty_st_new %>%
      full_join(cty_long)
    
    # Filter out any records without a GISJOIN
    ctys_new_to_write <- cty_st_new_join %>%
      filter(!is.na(current_year)) %>%
      select(!c(current_year, NAME_CTY_U, GISJOIN_CTY)) %>%
      rename(GISJOIN_CTY = GISJOIN)

    # Convert NAs to blanks
    ctys_new_to_write <- replace(ctys_new_to_write, is.na(ctys_new_to_write), " ")

    # Write CSV list
    write_csv(ctys_new_to_write, county_new_path)

    print(glue("County CSV written for {state}"))

  } else {

    print(glue("No county shapefile for {state}"))
    
    # Build dummy df for states that don't have this geography to use in full bind of old files
    cty_st_new_join <- data.frame()

  }
  
  ## County Subdivision

  if (file.exists(cousub_old_path)) {
    
    # Load county subdivision file from prior year
    cousub_long <- read_csv(cousub_old_path, col_types = cols(.default = col_character()))

    # Filter new county list to state
    cousub_st_new <- cousubs_new %>%
      filter(STATEA %in% cousub_long$STATEA)

    cousub_st_new_join <- cousub_st_new %>%
      full_join(cousub_long)
    
    cousub_new_to_write <- cousub_st_new_join %>%
      filter(!is.na(current_year)) %>%
      select(!c(current_year, NAME_CS_U, GISJOIN_CS)) %>%
      rename(GISJOIN_CS = GISJOIN)

    # Convert NAs to blanks
    cousub_new_to_write <- replace(cousub_new_to_write, is.na(cousub_new_to_write), " ")

    # Write CSV list
    write_csv(cousub_new_to_write, cousub_new_path)

    print(glue("County Subdivision CSV written for {state}"))

  } else {

    print(glue("No county subdivision shapefile for {state}"))
    
    # Build dummy df for states that don't have this geography to use in full bind of old files
    cousub_st_new_join <- data.frame()

  }


  ## Places

  if (file.exists(place_old_path)) {
    
    # Load place file from prior year
    place_long <- read_csv(place_old_path, col_types = cols(.default = col_character()))

    # Filter new county list to state
    place_st_new <- places_new %>%
      filter(STATEA %in% place_long$STATEA)

    # Join naccho ids from prior year to new place file
    place_st_new_join <- place_st_new %>%
      full_join(place_long)
    
    # Filter to only rows with a record in the new file
    place_new_to_write <- place_st_new_join %>%
      filter(!is.na(current_year)) %>%
      select(!c(current_year, NAME_PL_U, GISJOIN_PL)) %>%
      rename(GISJOIN_PL = GISJOIN)

    # Convert NAs to blanks
    place_new_to_write <- replace(place_new_to_write, is.na(place_new_to_write), " ")

    # Write CSV list
    write_csv(place_st_new_join, place_new_path)

    print(glue("Place CSV written for {state}"))

  } else {

    print(glue("No place shapefile for {state}, blank place CSV written"))
    
    # Build dummy df for states that don't have this geography to use in full bind of old files
    place_st_new_join <- data.frame()

  }
  
  return(list(cty_full_join = cty_st_new_join,
              cousub_full_join = cousub_st_new_join,
              place_full_join = place_st_new_join))

}

## ---- Write new long CSVs ---- 

# Run function to create csvs for all (or a select list) of states
# This function also reeturns df of all of the prior years long form CSVs for use later
update_fips_lists <- map(states, ~update_long_lists(.x))

## ---- Create and write lists of records lost in update ----

# Bind all rows - combine states to make nationwide file
cty_full_join <- bind_rows(map(update_fips_lists, ~.x$cty_full_join))
cousub_full_join <- bind_rows(map(update_fips_lists, ~.x$cousub_full_join))
place_full_join <- bind_rows(map(update_fips_lists, ~.x$place_full_join))

# Filter to only records with naccho_ids that are not in the new file
cty_lost_lhds <- cty_full_join %>%
  filter(is.na(current_year) & !is.na(naccho_id)) %>%
  select(GISJOIN_CTY, STATEA, COUNTYA, NAME_CTY, naccho_id, lhd_name)

cousub_lost_lhds <- cousub_full_join %>%
  filter(is.na(current_year) & !is.na(naccho_id)) %>%
  select(GISJOIN_CS, STATEA, COUNTYA, NAME_CTY, COUSUB, NAME_CS, naccho_id, lhd_name)

place_lost_lhds <- place_full_join %>%
  filter(is.na(current_year) & !is.na(naccho_id)) %>%
  select(GISJOIN_PL, STATEA, PLACE, NAME_PL, naccho_id, lhd_name)

# Write out lost records lists
write_csv(cty_lost_lhds, glue("{new_list_dir}/dropped_cty_records.csv"))
write_csv(cousub_lost_lhds, glue("{new_list_dir}/dropped_cousub_records.csv"))
write_csv(place_lost_lhds, glue("{new_list_dir}/dropped_place_records.csv"))

## ---- Copy denominator files from previous year ----
# File with separate columns and FIPS codes
file.copy(glue("{old_list_dir}/fips_denominator_{former_lhd_vintage}.csv"), glue("{new_list_dir}/fips_denominator_{former_lhd_vintage}.csv"))

# File with County, Cousub, and Place names concatenated
file.copy(glue("{old_list_dir}/denominator_{former_lhd_vintage}.csv"), glue("{new_list_dir}/denominator_{former_lhd_vintage}.csv"))

## ---- Delete API extract files ----
unlink(cty_temp_dir, recursive = TRUE)
unlink(cousub_temp_dir, recursive = TRUE)
unlink(place_temp_dir, recursive = TRUE)
