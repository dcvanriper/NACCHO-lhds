#######################################

## ---- build_new_lhd_lists.R ----
#
# Author: Kate Knowles
# Date: 2023-07-30
#
# This script does the following:
# 1. Creates directory for new update year
# 2. Builds new long format csvs (county, cousub, and place) for NACCHO to update
# 3. Identifies records lost (FIPS code changes) when they impact lhd coverage
# 4. Copies wide denominator files from prior year for reference
# 
# Prior to running this script, lhd_update_config.R should be edited and run.

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

## Submit extracts and select and rename columns

## Counties
new_counties <- submit_extract(cty_update_extract) %>%  # Submit extract
  wait_for_extract() %>%  # Wait until extract is ready
  download_extract(cty_temp_dir) %>%  # Download extract to directory created above
  read_nhgis()  # Read county table into R session

# Select attributes
ctys_new <- new_counties %>%
  select(GISJOIN_CTY = GISJOIN, 
         STATEA_U = STATEA, 
         COUNTYA_U = COUNTYA, 
         NAME_CTY_U = COUNTY)

## County subdivisions
new_cousubs <- submit_extract(cousub_update_extract) %>%
  wait_for_extract() %>%
  download_extract(cousub_temp_dir) %>%
  read_nhgis()

# Select attributes and filter
cousubs_new <- new_cousubs %>%
  select(GISJOIN_CS = GISJOIN, 
         STATEA_U = STATEA,
         COUNTYA_U = COUNTYA,
         NAME_CTY_U = COUNTY,
         COUSUB_U = COUSUBA, 
         NAME_CS_U = COUSUB) %>%
  filter(COUSUB_U != "00000" & COUSUB_U != "99999") # Eliminate records with no county subdivision (these are usually slivers around coasts and are records we do not need to include)

## Places
new_places <- submit_extract(place_update_extract) %>%
  wait_for_extract() %>%
  download_extract(place_temp_dir) %>%
  read_nhgis()

# Select attributes
places_new <- new_places %>%
  select(GISJOIN_PL = GISJOIN, 
         STATEA_U = STATEA,
         PLACE_U = PLACEA,
         NAME_PL_U = PLACE)

## ---- Setup to build update lists ----
# Vector of state abbreviations
states <- state.abb[!state.abb == "RI"] # RI is not part of NACCHO so removed
states <- c(states, "DC") # add DC to the list

## Build directory for new output lists

# Set update directory
old_list_dir <- glue("{dat_dir}/lists_to_update/{former_lhd_vintage}")
new_list_dir <- glue("{dat_dir}/lists_to_update/{lhd_vintage}")

# Check for new lhd year directory and build if it does not exist
if (dir.exists(new_list_dir)) {
  print(glue("{lhd_vintage} update directory already exists"))
} else {
  dir.create(new_list_dir)
  print(glue("{lhd_vintage} update directory created"))
}

## ---- Function to update long CSVs ----

# Function to add leading zeros to STATEA and COUNTYA for joins
lead_z <- function(x, n) {
  ifelse(
    str_count(x) <= n, 
    paste0(paste0(rep("0", n - str_count(x)), collapse = ""), x), 
    x
  )
}

# Function to write out updated long format CSV lists for updating
update_long_lists <- function(state_list)  {
  
  state <- state_list
  
  last_year_st_dir <- glue("{dat_dir}/lists_to_update/{former_lhd_vintage}/{state}")
  new_year_st_dir <- glue("{dat_dir}/lists_to_update/{lhd_vintage}/{state}")
  
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
    cty_long <- read_csv(county_old_path, col_types = cols(.default = col_character())) %>%
      select(!new_record) %>%
      mutate(STATEB = map_chr(STATEA, ~lead_z(.x, 2)))

    # Filter new county list to state
    cty_st_new <- ctys_new %>%
      filter(STATEA_U %in% cty_long$STATEB)

    # Join prior year naccho ids to current FIPS codes
    cty_st_new_join <- cty_st_new %>%
      full_join(cty_long, by = "GISJOIN_CTY") %>%
      mutate(new_record = ifelse(is.na(STATEA), "1", " ")) %>%
      relocate(new_record, .before = naccho_id)

    # Filter out any records not from the new year (lost records) - These are the lists to update
    ctys_new_to_write <- cty_st_new_join %>%
      filter(!is.na(STATEA_U)) %>%
      select(!c(STATEA, COUNTYA, NAME_CTY, STATEB)) %>%
      rename(STATEA = STATEA_U,
             COUNTYA = COUNTYA_U,
             NAME_CTY = NAME_CTY_U)

    # Convert NAs to blanks
    ctys_new_to_write <- replace(ctys_new_to_write, is.na(ctys_new_to_write), " ")

    # Wipe notes field clean
    ctys_new_to_write <- ctys_new_to_write %>%
      mutate(notes = " ")

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
    cousub_long <- read_csv(cousub_old_path, col_types = cols(.default = col_character())) %>%
      select(!new_record) %>%
      mutate(STATEB = map_chr(STATEA, ~lead_z(.x, 2)))

    # Filter new county list to state
    cousub_st_new <- cousubs_new %>%
      filter(STATEA_U %in% cousub_long$STATEB)
    
    # Join old naccho ids to new county subdivision records
    cousub_st_new_join <- cousub_st_new %>%
      full_join(cousub_long) %>%
      mutate(new_record = ifelse(is.na(STATEA), "1", " ")) %>%
      relocate(new_record, .before = naccho_id)
    
    cousub_new_to_write <- cousub_st_new_join %>%
      filter(!is.na(STATEA_U)) %>%
      select(!c(STATEA, COUNTYA, NAME_CTY, NAME_CS, COUSUB, STATEB)) %>%
      rename(STATEA = STATEA_U,
             COUNTYA = COUNTYA_U,
             NAME_CTY = NAME_CTY_U,
             COUSUB = COUSUB_U,
             NAME_CS = NAME_CS_U)

    # Convert NAs to blanks
    cousub_new_to_write <- replace(cousub_new_to_write, is.na(cousub_new_to_write), " ")
    
    # Wipe notes field clean
    cousub_new_to_write <- cousub_new_to_write %>% 
      mutate(notes = " ")
    
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
  place_long <- read_csv(place_old_path, col_types = cols(.default = col_character())) %>%
    select(!new_record) %>%
    mutate(STATEB = map_chr(STATEA, ~lead_z(.x, 2)))

  # Filter new county list to state
  place_st_new <- places_new %>%
    filter(STATEA_U %in% place_long$STATEA)

  # Join naccho ids from prior year to new place file
  place_st_new_join <- place_st_new %>%
    full_join(place_long) %>%
    mutate(new_record = ifelse(is.na(STATEA), "1", " ")) %>%
    relocate(new_record, .before = naccho_id)

  # Filter to only rows with a record in the new file
  place_new_to_write <- place_st_new_join %>%
    filter(!is.na(STATEA_U)) %>%
    select(!c(STATEA, PLACE, NAME_PL, STATEB)) %>%
    rename(STATEA = STATEA_U,
           PLACE = PLACE_U,
           NAME_PL = NAME_PL_U)

  # Convert NAs to blanks
  place_new_to_write <- replace(place_new_to_write, is.na(place_new_to_write), " ")

  # Wipe notes field clean
  place_new_to_write <- place_new_to_write %>%
    mutate(notes = " ")

  # Write CSV list
  write_csv(place_new_to_write, place_new_path)

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
  filter(is.na(STATEA_U) & !is.na(naccho_id)) %>%
  select(GISJOIN_CTY, STATEA, COUNTYA, NAME_CTY, naccho_id, lhd_name, notes)

cousub_lost_lhds <- cousub_full_join %>%
  filter(is.na(STATEA_U) & !is.na(naccho_id)) %>%
  select(GISJOIN_CS, STATEA, COUNTYA, NAME_CTY, COUSUB, NAME_CS, naccho_id, lhd_name, notes)

place_lost_lhds <- place_full_join %>%
  filter(is.na(STATEA_U) & !is.na(naccho_id)) %>%
  select(GISJOIN_PL, STATEA, PLACE, NAME_PL, naccho_id, lhd_name, notes)

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
