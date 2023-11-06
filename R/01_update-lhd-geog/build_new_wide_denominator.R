#######################################

## ---- build_wide_denominator.R ----
#
# Author: Kate Knowles
# Date: 2023-07-26
#
# This script builds one wide file from the long format csvs (county, cousub, and place) for NACCHO's records.

####################################### 

## ---- Load packages ----
library(tidyverse)
library(glue)

## ---- Set constants ----

# Vector of state abbreviations
states <- state.abb[!state.abb == "RI"] # RI is not part of NACCHO so removed
states <- c(states, "DC") # add DC to the list

# Function to Load and pivot all county files 
ctys_load <-  function(state_list) {
  
  state <- state_list
  tbl_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/{state}")
  cty_long_path <- glue("{tbl_dir}/{state}_county_lhds.csv")
  
  if (file.exists(cty_long_path)) {
    
    cty_long <- read_csv(cty_long_path, col_types = cols(.default = col_character())) %>%
      select(STATEA, 
             NAME_CTY, 
             COUNTYA, 
             naccho_id, 
             lhd_name) %>%
      filter(!is.na(naccho_id))
    
    print(glue("County CSV exists for {state}."))
    
    return(cty_long = cty_long)
    
  }
  
}

# Pivot all long County CSVs
st_ctys_long <- map(states, ~ctys_load(.x))

ctys_long <- bind_rows(st_ctys_long)

ctys_wide <- ctys_long %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = id, values_from = c(NAME_CTY, COUNTYA))

ctys_concat <- ctys_long %>%
  select(-COUNTYA) %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  summarise(COUNTIES = paste(NAME_CTY, collapse = ", "))

## ---- County Subdividions ----

# Function to Load and pivot all cousub files

cousubs_load <- function(state_list) {
  
  state <- state_list
  tbl_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/{state}")
  cousub_long_path <- glue("{tbl_dir}/{state}_cousub_lhds.csv")
  
  if (file.exists(cousub_long_path)) {
    
    cousub_long <- read_csv(cousub_long_path, col_types = cols(.default = col_character())) %>%
      select(STATEA, 
             NAME_CS,
             COUSUB,
             naccho_id, 
             lhd_name)
    
    cousub_long_lhds <- cousub_long %>%
      filter(!is.na(naccho_id))
    
    print(glue("County Subdivision CSV exists for {state}."))
    
    return(cousub_long_lhds = cousub_long_lhds)
  }
}

st_cousubs_long <- map(states, ~cousubs_load(.x))

cousubs_long <- bind_rows(st_cousubs_long)

cousubs_wide <- cousubs_long %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = id, values_from = c(NAME_CS, COUSUB))

cousubs_concat <- cousubs_long %>%
  select(-COUSUB) %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  summarise(COUSUBS = paste(NAME_CS, collapse = ", "))


## ---- Places ----

# Function to Load and pivot all place files 

places_load <- function(state_list) {
  
  state <- state_list
  tbl_dir <- glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/{state}")
  place_long_path <- glue("{tbl_dir}/{state}_place_lhds.csv")
  
  if (file.exists(place_long_path)) {
    
    place_long <- read_csv(place_long_path, col_types = cols(.default = col_character())) %>%
      select(STATEA,
             NAME_PL,
             PLACE,
             naccho_id,
             lhd_name)
    
    place_long_lhds <- place_long %>%
      filter(!is.na(naccho_id))
    
    print(glue("Place CSV exists for {state}."))
    
    return(place_long_lhds = place_long_lhds)
  }
  
}

st_places_long <- map(states, ~places_load(.x))

places_long <- bind_rows(st_places_long)

places_wide <- places_long %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = id, values_from = c(NAME_PL, PLACE))

places_concat <- places_long %>%
  select(-PLACE) %>%
  group_by(STATEA, naccho_id, lhd_name) %>%
  summarise(PLACES = paste(NAME_PL, collapse = ", "))

## ---- Build one wide file ----

## Full file w/ separate columns and FIPS codes

# Join Counties and Cousubs
ctys_cousubs_wide <- ctys_wide %>%
  full_join(cousubs_wide)

# Join place onto cousub/county (full_join by naccho_id)
ctys_cousubs_places_wide <- ctys_cousubs_wide %>%
  full_join(places_wide) %>%
  arrange(STATEA,
          naccho_id)

# Get rid of NAs
denom_wide <- replace(ctys_cousubs_places_wide, is.na(ctys_cousubs_places_wide), "") 

## Concatenated file with only names and condensed columns

# Join Counties and Cousubs
ctys_cousubs_concat <- ctys_concat %>%
  full_join(cousubs_concat)

# Join place onto cousub/county
ctys_cousubs_places_concat <- ctys_cousubs_concat %>%
  full_join(places_concat) %>%
  arrange(STATEA, 
          naccho_id)

# Get rid of NAs
denom_concat <- replace(ctys_cousubs_places_concat, is.na(ctys_cousubs_places_concat), "")

## ---- Write out final wide files ----

# Write out single denominator file
write_csv(denom_wide, glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/fips_denominator_{lhd_vintage}.csv"))

# Write out single denominator file w/ names concatenated into three columns
write_csv(denom_concat, glue("{dat_dir}/tables/lists_to_update/{lhd_vintage}/denominator_{lhd_vintage}.csv"))
