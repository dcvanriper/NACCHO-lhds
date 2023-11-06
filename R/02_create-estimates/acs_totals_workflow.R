# acs_totals_workflow.R
# ACS totals workflow
# 
# This script checks to see if the ACS Place070 dataset exists. If it exists,
# it loads in the data.
# 
# This script then iterates over individual states, loading in CSVs, and merging them
# with the Place070 data frame created by the acs_place070_prep.R script. It assigns an LHD
# code (naccho_id) to records in the Place070 file and then creates a state-level dataset by LHD 
# containing an estimate for the following:
# 
# 1. Total population
# 2. Population by race
#   - white alone
#   - black alone
#   - american indian or alaska native alone
#   - asian alone
#   - native hawaiian or other pacific islander alone
#   - some other race alone
#   - two or more races
# 3. Housing units
# 4. Owner occupied housing units
# 5. Renter occupied housing units  
# 6. Households
# 

require(tidyverse)
require(vroom)
require(glue)
require(sf)
require(ipumsr) # This is using the dev version as of 2023-05-03

options(scipen = 999)

## Check to see if directory exists. If it doesn't, create it. 
if(!dir.exists(download_acs_totals_dir)){
  dir.create(download_acs_totals_dir, recursive = TRUE)
}

## Check to see if there is an .zip file in the download directory. If there isn't, run the extract.
if(length(list.files(download_acs_totals_dir)) == 0){
  # Define and submit extract
  define_extract_nhgis(
    description = glue("Place070, {acs_dataset}"),
    datasets = ds_spec(
      acs_dataset,
      data_tables = place070_data_tables,
      geog_levels = "place_070")
    ) |>
    submit_extract() |>
    wait_for_extract() |>
    download_extract(download_acs_totals_dir)
}

## ---- Load the ACS extract containing the totals ----
## This assumes that create_download_acs_totals_place070.R has been run.
file <- list.files(download_acs_totals_dir, "zip", full.names = TRUE)

place070 <- read_nhgis(file)

## ---- Process the place070 data frame to standardize the column headers ----
## Get metadata for the tables
meta_totpop <- get_metadata_nhgis(dataset = acs_dataset, data_table = place070_data_tables[1])
meta_race <- get_metadata_nhgis(dataset = acs_dataset, data_table = place070_data_tables[2])
meta_hu <- get_metadata_nhgis(dataset = acs_dataset, data_table = place070_data_tables[3])
meta_hh <- get_metadata_nhgis(dataset = acs_dataset, data_table = place070_data_tables[4])

# Rename the variables in the place070 data frame
place070 <- place070 |>
  rename(totpop = contains(glue(meta_totpop$nhgis_code, "E001")),
         race_white_alone = contains(glue(meta_race$nhgis_code, "E002")),
         race_black_alone = contains(glue(meta_race$nhgis_code, "E003")),
         race_aian_alone = contains(glue(meta_race$nhgis_code, "E004")),
         race_asian_alone = contains(glue(meta_race$nhgis_code, "E005")),
         race_nhopi_alone = contains(glue(meta_race$nhgis_code, "E006")),
         race_sor_alone = contains(glue(meta_race$nhgis_code, "E007")),
         race_two_more = contains(glue(meta_race$nhgis_code, "E008")),
         hu = contains(glue(meta_hu$nhgis_code, "E001")),
         hh = contains(glue(meta_hh$nhgis_code, "E001")),
         hh_own = contains(glue(meta_hh$nhgis_code, "E002")),
         hh_rent = contains(glue(meta_hh$nhgis_code, "E003")))

# Retain required fields from place070 data frame
place070 <- place070 |>
  select(GISJOIN, STUSAB, STATEA, COUNTYA, COUSUBA, PLACEA, NAME_E, totpop, starts_with("race"), hu, starts_with("hh"))

## ---- Function to generate ACS totals by state ----
generate_acs_totals <- function(st){
  
  print(st)
  
  # Process single state
  st_place070 <- place070 |>
    filter(STUSAB == st)
  
  # load place LHDs - there will always be a place LHD CSV
  pl_lhds <- read_csv(glue("{dat_dir}/lists_to_update/{lhd_vintage}/{st}/{st}_place_lhds.csv"), col_types = "cccccccc") |> 
    mutate(STATEA = str_sub(GISJOIN_PL, 2, 3),
           PLACE = str_sub(GISJOIN_PL, 5, 9))
  
  # if exists, load county lhds  
  if(file.exists(glue("{dat_dir}/lists_to_update/{lhd_vintage}/{st}/{st}_county_lhds.csv"))){
    county_lhds <- read_csv(glue("{dat_dir}/lists_to_update/{lhd_vintage}/{st}/{st}_county_lhds.csv"), col_types = "cccccccc") |> 
      mutate(STATEA = str_sub(GISJOIN_CTY, 2, 3),
             COUNTYA = str_sub(GISJOIN_CTY, 5, 7))
  }
  
  # if exists, load county subdivision lhds  
  if(file.exists(glue("{dat_dir}/lists_to_update/{lhd_vintage}/{st}/{st}_cousub_lhds.csv"))){
    cousub_lhds <- read_csv(glue("{dat_dir}/lists_to_update/{lhd_vintage}/{st}/{st}_cousub_lhds.csv"), col_types = "ccccccccc") |> 
      mutate(STATEA = str_sub(GISJOIN_CS, 2, 3),
             COUNTYA = str_sub(GISJOIN_CS, 5, 7),
             COUSUB = str_sub(GISJOIN_CS, 9, 13))
  }

  ## --- Assigns LHDs to records in Place070 ----

  ## Join place LHDs onto place070 data frame
  st_place070_pl <- st_place070 |> 
    left_join(pl_lhds, by = c("STATEA" = "STATEA", "PLACEA" = "PLACE")) |> 
    rename(naccho_id_place = naccho_id)
  
  ## Join the county LHD onto the st_place070_pl data frame if the county LHD exists
  if(exists("county_lhds")){
    st_place070_pl_county <- st_place070_pl |> 
      left_join(county_lhds, by = c("STATEA" = "STATEA", "COUNTYA" = "COUNTYA")) |> 
      rename(naccho_id_county = naccho_id)
  } else {
    st_place070_pl_county <- st_place070_pl |> 
      mutate(naccho_id_county = NA_character_)
  }

  ## Join the county subdivision-based LHDs onto st_place070_pl_county if the county subdivision LHD object exists. If 
  ## it doesn't exist, add a naccho_id_county = NA_character_ variable to the st_place070_pl_county dataframe
  if(exists("cousub_lhds")){
    st_place070_pl_county <- st_place070_pl_county |> 
      left_join(cousub_lhds, by = c("STATEA" = "STATEA", "COUNTYA" = "COUNTYA", "COUSUBA" = "COUSUB"))  |> 
      rename(naccho_id_cousub = naccho_id)
  } else {
    st_place070_pl_county <- st_place070_pl_county |>
      mutate(naccho_id_cousub = NA_character_)
  }
  
  ## Create a new naccho_id keeping place over county and cousub
  st_place070_pl_county <- st_place070_pl_county |> 
    mutate(naccho_id = case_when(!is.na(naccho_id_place) ~ naccho_id_place,
                                 !is.na(naccho_id_cousub) ~ naccho_id_cousub,
                                 !is.na(naccho_id_county) ~ naccho_id_county))
  
  ## ---- Compute the LHD ACS counts for variables available by Place070 ----
  st_lhd_ACS <- st_place070_pl_county |>
    group_by(naccho_id) |>
    summarise(totpop = sum(totpop),
              race_white_alone = sum(race_white_alone),
              race_black_alone = sum(race_black_alone),
              race_aian_alone = sum(race_aian_alone),
              race_asian_alone = sum(race_asian_alone),
              race_nhopi_alone = sum(race_nhopi_alone),
              race_sor_alone = sum(race_sor_alone),
              race_two_more = sum(race_two_more),
              hu = sum(hu),
              hh = sum(hh),
              hh_own = sum(hh_own),
              hh_rent = sum(hh_rent)) |> 
    mutate(STUSAB = st) #|>
    #filter(!is.na(naccho_id))
  
  ## ---- Bind state-specific tbls into one nationwide tbl ----
  #if(exists("acs")){
  #  acs <- bind_rows(acs, st_lhd_ACS)
  #} else {
  #  acs <- st_lhd_ACS
  #}
  
  ## ---- Create output files for NACCHO ----
  #write_csv(st_lhd_ACS, glue("/Users/{user}/Box Sync/NACCHO GIS/data_working/tables/{acs_dataset}/acs_totals/{st}.csv"))
  
  ## ---- Remove LHDs dfs if they exist ----
  if(exists("pl_lhds")){
    rm(pl_lhds)
  }
  
  if(exists("cousub_lhds")){
    rm(cousub_lhds)
  }
  
  if(exists("county_lhds")){
    rm(county_lhds)
  }
  
  return(st_lhd_ACS)
}

## ---- Iterate over all states to generate ACS totals ----
acs_totals_by_state <- map(state_abbr, ~generate_acs_totals(.x))

## ---- Convert out to a tibble using bind_rows ----
acs_totals <- bind_rows(acs_totals_by_state)

## ---- Drop the records with naccho_id = NA and write out the new data frame to CSVs
acs_final <- acs_totals |> 
  filter(!is.na(naccho_id))

## ---- Create nationwide output files for NACCHO ----
write_csv(acs_final, glue("{acs_output_dir}/{lhd_vintage}_acstotals.csv"))

## ---- Create the Place070 weighting variables ----
## Create the county subdivision totals to serve as the denominator
cousub_totals <- place070 |>
  group_by(STATEA, COUNTYA, COUSUBA) |>
  summarise(cousub_totpop = sum(totpop),
            cousub_hu = sum(hu),
            cousub_hh = sum(hh))

## Create a final Place070 weighting variables by dividing the Place070 counts by the appropriate county subdivision count
place070 <- place070 |>
  left_join(cousub_totals) |>
  mutate(perc_totpop = totpop / cousub_totpop,
         perc_totpop = case_when(is.nan(perc_totpop) ~ 0,
                                 TRUE ~ perc_totpop),
         perc_hu = hu / cousub_hu,
         perc_hu = case_when(is.nan(perc_hu) ~ 0,
                             TRUE ~ perc_hu),
         perc_hh = hh / cousub_hh,
         perc_hh = case_when(is.nan(perc_hh) ~ 0,
                             TRUE ~ perc_hh)) |>
  select(-starts_with("cousub_"))

