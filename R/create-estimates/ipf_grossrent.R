# ipf_grossrent.R
# This script attempts to generate LHD estimates of binned renter-occupied housing unit gross rent
# using iterative proportional fitting.

require(tidyverse)
require(vroom)
require(glue)
require(ipumsr) # This is using the dev version as of 2023-05-03

## ---- Create and download an ACS 5-year extract for the renter-occupied housing units gross rent IPF ----
# If the directory in the data object doesn't exist, the next code snipped will create the directory
if(!dir.exists(download_acs_grossrent_dir)){
  dir.create(download_acs_grossrent_dir, recursive= TRUE)
}

# Define and submit extract
if(length(list.files(download_acs_grossrent_dir)) == 0){
  define_extract_nhgis(
    description = glue("County Subdivision & Place Gross Rent Value - {acs_dataset}"),
    datasets = ds_spec(acs_dataset,
                       data_tables = acs_grossrent,
                       geog_levels = c("cty_sub", "place")
                       )) |>
    submit_extract() |>
    wait_for_extract() |>
    download_extract(download_acs_grossrent_dir)
}

## ---- Load the ACS extract containing the gross rent data ----
file <- list.files(download_acs_grossrent_dir, "zip", full.names = TRUE)

# Read in the ACS data
acs_cousub <- read_nhgis(file, file_select = contains("cty_"))
acs_place <- read_nhgis(file, file_select = contains("place"))

## ---- Metadata ----
# Get table-metadata for the four Place070 tables
meta <- get_metadata_nhgis(dataset = acs_dataset, data_table = acs_grossrent)

# Get variables for gross rent table
meta_variables <- meta$variables

# Create descriptive vars from descriptoin and add E to the nhgis_codes
meta_variables <- meta_variables |>
  mutate(nhgis_code = paste0(str_sub(nhgis_code, 1,4), "E", str_sub(nhgis_code, 5,8)),
         description = str_remove(description, "With cash rent: "),
         description = str_replace_all(description, "[ ]", "_"),
         description = str_replace_all(description, "[,]", ""),
         description = str_replace_all(description, "[$]", "rent_"),
         description = tolower(description))

# Create vectors for the rename_with function
recode_key <- purrr::set_names(c(meta_variables$description), c(meta_variables$nhgis_code))

# Rename columns to generic names
acs_cousub <- acs_cousub |>
  rename_with(~recode(colnames(acs_cousub), !!!recode_key))

acs_place <- acs_place |>
  rename_with(~recode(colnames(acs_place), !!!recode_key))

# Retain required fields for sex
acs_cousub_grossrent <- acs_cousub |>
  select(STUSAB, STATEA, COUNTYA, COUSUBA, less_than_rent_100:no_cash_rent)

acs_place_grossrent <- acs_place |>
  select(STUSAB, STATEA, PLACEA, less_than_rent_100:no_cash_rent)


## ---- Function to generate ACS renter-occupied housing unit gross rent IPF estimates by state ----
generate_acs_grossrent_ipf <- function(st){
  
  print(st)
  
  # Filter acs_totals (by LHD) to a single state
  st_lhd_ACS <- acs_totals |> 
    filter(STUSAB == st)
  
  # Filter place070 to a single state
  st_place070 <- place070 |>
    filter(STUSAB == st)
  
  # Filter acs_totals to a single state, retaining the tenure counts, naccho_id and STUSAB
  ipf_tenure <- acs_totals |>
    select(naccho_id, hh_own, hh_rent, STUSAB) |> 
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
  
  # if exists, load county lhds  
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
  
  
  ## ---- Create LHD Renter-occupied Gross Rent estimates ----
  # st_place070_pl_county has the weights so join the acs_cousub_grossrent df onto the st_place070_pl_county df
  st_place070_pl_county <- st_place070_pl_county |>
    left_join(acs_cousub_grossrent) |>
    mutate(est_less_than_rent_100 = perc_hh * less_than_rent_100,
           est_rent_100_to_rent_149 = perc_hh * rent_100_to_rent_149,
           est_rent_150_to_rent_199 = perc_hh * rent_150_to_rent_199,
           est_rent_200_to_rent_249 = perc_hh * rent_200_to_rent_249,
           est_rent_250_to_rent_299 = perc_hh * rent_250_to_rent_299,
           est_rent_300_to_rent_349 = perc_hh * rent_300_to_rent_349,
           est_rent_350_to_rent_399 = perc_hh * rent_350_to_rent_399,
           est_rent_400_to_rent_449 = perc_hh * rent_400_to_rent_449,
           est_rent_450_to_rent_499 = perc_hh * rent_450_to_rent_499,
           est_rent_500_to_rent_549 = perc_hh * rent_500_to_rent_549,
           est_rent_550_to_rent_599 = perc_hh * rent_550_to_rent_599,
           est_rent_600_to_rent_649 = perc_hh * rent_600_to_rent_649,
           est_rent_650_to_rent_699 = perc_hh * rent_650_to_rent_699,
           est_rent_700_to_rent_749 = perc_hh * rent_700_to_rent_749,
           est_rent_750_to_rent_799 = perc_hh * rent_750_to_rent_799,
           est_rent_800_to_rent_899 = perc_hh * rent_800_to_rent_899,
           est_rent_900_to_rent_999 = perc_hh * rent_900_to_rent_999,
           est_rent_1000_to_rent_1249 = perc_hh * rent_1000_to_rent_1249,
           est_rent_1250_to_rent_1499 = perc_hh * rent_1250_to_rent_1499,
           est_rent_1500_to_rent_1999 = perc_hh * rent_1500_to_rent_1999,
           est_rent_2000_to_rent_2499 = perc_hh * rent_2000_to_rent_2499,
           est_rent_2500_to_rent_2999 = perc_hh * rent_2500_to_rent_2999,
           est_rent_3000_to_rent_3499 = perc_hh * rent_3000_to_rent_3499,
           est_rent_3500_or_more = perc_hh * rent_3500_or_more,
           est_no_cash_rent = perc_hh * no_cash_rent) 
  
  # generate LHD gross rent estimates based on cousub-weighted values
  st_lhd_grossrent <- st_place070_pl_county |>
    group_by(naccho_id) |>
    summarise(less_than_rent_100 = sum(est_less_than_rent_100),
              rent_100_to_rent_149 = sum(est_rent_100_to_rent_149),
              rent_150_to_rent_199 = sum(est_rent_150_to_rent_199),
              rent_200_to_rent_249 = sum(est_rent_200_to_rent_249),
              rent_250_to_rent_299 = sum(est_rent_250_to_rent_299),
              rent_300_to_rent_349 = sum(est_rent_300_to_rent_349),
              rent_350_to_rent_399 = sum(est_rent_350_to_rent_399),
              rent_400_to_rent_449 = sum(est_rent_400_to_rent_449),
              rent_450_to_rent_499 = sum(est_rent_450_to_rent_499),
              rent_500_to_rent_549 = sum(est_rent_500_to_rent_549),
              rent_550_to_rent_599 = sum(est_rent_550_to_rent_599),
              rent_600_to_rent_649 = sum(est_rent_600_to_rent_649),
              rent_650_to_rent_699 = sum(est_rent_650_to_rent_699),
              rent_700_to_rent_749 = sum(est_rent_700_to_rent_749),
              rent_750_to_rent_799 = sum(est_rent_750_to_rent_799),
              rent_800_to_rent_899 = sum(est_rent_800_to_rent_899),
              rent_900_to_rent_999 = sum(est_rent_900_to_rent_999),
              rent_1000_to_rent_1249 = sum(est_rent_1000_to_rent_1249),
              rent_1250_to_rent_1499 = sum(est_rent_1250_to_rent_1499),
              rent_1500_to_rent_1999 = sum(est_rent_1500_to_rent_1999),
              rent_2000_to_rent_2499 = sum(est_rent_2000_to_rent_2499),
              rent_2500_to_rent_2999 = sum(est_rent_2500_to_rent_2999),
              rent_3000_to_rent_3499 = sum(est_rent_3000_to_rent_3499),
              rent_3500_or_more = sum(est_rent_3500_or_more),
              no_cash_rent = sum(est_no_cash_rent))
  
  ## ---- Create gross rent counts for place-based LHDs ----
  # collapse multi-place lhds first
  if(exists("pl_lhds")){
    pl_lhds_grossrent <- pl_lhds |>
      left_join(acs_place_grossrent, by = c("STATEA" = "STATEA", "PLACE" = "PLACEA")) |>
      group_by(naccho_id) |>
      summarise(less_than_rent_100 = sum(less_than_rent_100),
                rent_100_to_rent_149 = sum(rent_100_to_rent_149),
                rent_150_to_rent_199 = sum(rent_150_to_rent_199),
                rent_200_to_rent_249 = sum(rent_200_to_rent_249),
                rent_250_to_rent_299 = sum(rent_250_to_rent_299),
                rent_300_to_rent_349 = sum(rent_300_to_rent_349),
                rent_350_to_rent_399 = sum(rent_350_to_rent_399),
                rent_400_to_rent_449 = sum(rent_400_to_rent_449),
                rent_450_to_rent_499 = sum(rent_450_to_rent_499),
                rent_500_to_rent_549 = sum(rent_500_to_rent_549),
                rent_550_to_rent_599 = sum(rent_550_to_rent_599),
                rent_600_to_rent_649 = sum(rent_600_to_rent_649),
                rent_650_to_rent_699 = sum(rent_650_to_rent_699),
                rent_700_to_rent_749 = sum(rent_700_to_rent_749),
                rent_750_to_rent_799 = sum(rent_750_to_rent_799),
                rent_800_to_rent_899 = sum(rent_800_to_rent_899),
                rent_900_to_rent_999 = sum(rent_900_to_rent_999),
                rent_1000_to_rent_1249 = sum(rent_1000_to_rent_1249),
                rent_1250_to_rent_1499 = sum(rent_1250_to_rent_1499),
                rent_1500_to_rent_1999 = sum(rent_1500_to_rent_1999),
                rent_2000_to_rent_2499 = sum(rent_2000_to_rent_2499),
                rent_2500_to_rent_2999 = sum(rent_2500_to_rent_2999),
                rent_3000_to_rent_3499 = sum(rent_3000_to_rent_3499),
                rent_3500_or_more = sum(rent_3500_or_more),
                no_cash_rent = sum(no_cash_rent)) |> 
      filter(!is.na(naccho_id))
    
    pl_lhds_grossrent <- pl_lhds_grossrent
    
    
    ## ---- Replace estimates in st_lhd_grossrent by values in pl_lhds_grossrent ----
    st_lhd_grossrent_drop <- st_lhd_grossrent |>
      filter(!naccho_id %in% pl_lhds_grossrent$naccho_id)
    
    
    # bind place-based LHDs with remainder
    st_lhd_grossrent_bind <- bind_rows(st_lhd_grossrent_drop, pl_lhds_grossrent) |>
      arrange(naccho_id)
  } else{
    st_lhd_grossrent_bind <- st_lhd_grossrent |> 
      arrange(naccho_id)
  }
  
  ## ---- Create household income totals for Ohio
  st_state_grossrent <- acs_cousub_grossrent |>
    filter(STUSAB == st) |>
    summarise(less_than_rent_100 = sum(less_than_rent_100),
              rent_100_to_rent_149 = sum(rent_100_to_rent_149),
              rent_150_to_rent_199 = sum(rent_150_to_rent_199),
              rent_200_to_rent_249 = sum(rent_200_to_rent_249),
              rent_250_to_rent_299 = sum(rent_250_to_rent_299),
              rent_300_to_rent_349 = sum(rent_300_to_rent_349),
              rent_350_to_rent_399 = sum(rent_350_to_rent_399),
              rent_400_to_rent_449 = sum(rent_400_to_rent_449),
              rent_450_to_rent_499 = sum(rent_450_to_rent_499),
              rent_500_to_rent_549 = sum(rent_500_to_rent_549),
              rent_550_to_rent_599 = sum(rent_550_to_rent_599),
              rent_600_to_rent_649 = sum(rent_600_to_rent_649),
              rent_650_to_rent_699 = sum(rent_650_to_rent_699),
              rent_700_to_rent_749 = sum(rent_700_to_rent_749),
              rent_750_to_rent_799 = sum(rent_750_to_rent_799),
              rent_800_to_rent_899 = sum(rent_800_to_rent_899),
              rent_900_to_rent_999 = sum(rent_900_to_rent_999),
              rent_1000_to_rent_1249 = sum(rent_1000_to_rent_1249),
              rent_1250_to_rent_1499 = sum(rent_1250_to_rent_1499),
              rent_1500_to_rent_1999 = sum(rent_1500_to_rent_1999),
              rent_2000_to_rent_2499 = sum(rent_2000_to_rent_2499),
              rent_2500_to_rent_2999 = sum(rent_2500_to_rent_2999),
              rent_3000_to_rent_3499 = sum(rent_3000_to_rent_3499),
              rent_3500_or_more = sum(rent_3500_or_more),
              no_cash_rent = sum(no_cash_rent)) |>
    pivot_longer(cols = less_than_rent_100:no_cash_rent, values_to = "v1", names_to = "value")
  
  ## ---- IPF for gross rent value estimates ----
  ## row = st_ipf_tenure (renter-occupied housing unit counts for each LHD)
  ## seed = st_lhd_grossrent_bind (renter-occupied rent estimates for each LHDs)
  ## col = st_state_grossrent
  ## 
  ## The rounded value for the LHD-level renter-occupied count does not sum correctly to the 
  ## state-level rent-occupied count for Ohio (at least). Thus, the ipf2 function will
  ## not run. 
  
  seed <- st_lhd_grossrent_bind |> 
    select(less_than_rent_100:no_cash_rent)
  rowc <- as.matrix(ipf_tenure$hh_rent)
  colc <- as.matrix(st_state_grossrent$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  if(sum(rowc) == sum(colc)){
    st_ipf_grossrent <- ipf2(rowc, colc, seed)$fitted.table
    st_ipf_grossrent<- bind_cols(select(st_lhd_grossrent_bind, naccho_id), st_ipf_grossrent)  |> 
      mutate(ipf_grossrent = 1,
             STUSAB = st)
  } else{
    st_ipf_grossrent <- bind_cols(select(st_lhd_grossrent_bind, naccho_id), seed) |> 
      mutate(ipf_grossrent = 0,
             STUSAB = st)
  }

  # Remove extraneous df
  if(exists("pl_lhds")){
    rm(pl_lhds)
  }
  
  ## Return st_ipf_grossrent
  return(st_ipf_grossrent)

}

## ---- Iterate over all states to generate ACS renter-occupied gross rent counts ----
acs_grossrent_by_state <- map(state_abbr, ~generate_acs_grossrent_ipf(.x)) ## Issue with something being character and something being logical (naccho_id)

## ---- Convert out to a tibble using bind_rows ----
acs_grossrent <- bind_rows(acs_grossrent_by_state) 

## ---- Drop NA records from acs_ownoccvalue before writing out to CSV ----
acs_grossrent <- acs_grossrent |> 
  filter(!is.na(naccho_id))

## ---- Create nationwide output file for renter-occupied gross rent ----
write_csv(acs_grossrent, glue("{acs_output_dir}/{lhd_vintage}_grossrent.csv"))