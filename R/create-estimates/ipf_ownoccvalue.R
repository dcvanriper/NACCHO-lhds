# ipf_ownoccvalue.R
# This script attempts to generate LHD estimates of binned owner-occupied housing unit values
# using iterative proportional fitting.

require(tidyverse)
require(vroom)
require(glue)
require(ipumsr)

## ---- Create and download an ACS 5-year extract for the owner-occupied housing units value IPF ----
# If the directory in the data object doesn't exist, the next code snipped will create the directory
if(!dir.exists(download_acs_ownoccvalue_dir)){
  dir.create(download_acs_ownoccvalue_dir, recursive= TRUE)
}

if(length(list.files(download_acs_ownoccvalue_dir)) == 0){
  # create and submit the NHGIS extract
  define_extract_nhgis(
    description = glue("County Subdivision & Place Housing Tenure - {acs_dataset}"),
    datasets = ds_spec(
      acs_dataset,
      data_tables = acs_ownoccvalue,
      geog_levels = c("cty_sub", "place")
    )) |> 
    submit_extract() |> 
    wait_for_extract() |> 
    download_extract(download_acs_ownoccvalue_dir)
}

## ---- Load the ACS extract containing the owner occupied value data ----
file <- list.files(download_acs_ownoccvalue_dir, "zip", full.names = TRUE)

# Read in the ACS data
acs_cousub <- read_nhgis(file, file_select = contains("cty_"))
acs_place <- read_nhgis(file, file_select = contains("place"))

## ---- Metadata ----
# Get table-metadata for the four Place070 tables
meta <- get_metadata_nhgis(dataset = acs_dataset, data_table = acs_ownoccvalue)

# Get variables for owner occupied value table
meta_variables <- meta$variables

# Create descriptive vars from descriptoin and add E to the nhgis_codes
meta_variables <- meta_variables |>
  mutate(nhgis_code = paste0(str_sub(nhgis_code, 1,4), "E", str_sub(nhgis_code, 5,8)),
         description = str_remove(description, ":"),
         description = str_replace_all(description, "[ ]", "_"),
         description = str_replace_all(description, "[,]", ""),
         description = str_replace_all(description, "[$]", "val_"),
         description = tolower(description))

# Create vectors for the rename_with function
recode_key <- purrr::set_names(c(meta_variables$description), c(meta_variables$nhgis_code))

# Rename columns to generic names
acs_cousub <- acs_cousub |>
  rename_with(~recode(colnames(acs_cousub), !!!recode_key))

acs_place <- acs_place |>
  rename_with(~recode(colnames(acs_place), !!!recode_key))

# Retain required fields for sex
acs_cousub_ownoccvalue <- acs_cousub |>
  select(STUSAB, STATEA, COUNTYA, COUSUBA, less_than_val_10000:val_2000000_or_more)

acs_place_ownoccvalue <- acs_place |>
  select(STUSAB, STATEA, PLACEA, less_than_val_10000:val_2000000_or_more)

## ---- Function to generate ACS owner-occupied housing unit values IPF estimates by state ----
generate_acs_ownoccvalue_ipf <- function(st){
  
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
  pl_lhds <- read_csv(glue("{dat_dir}/tables/lists_to_update/{st}/{st}_place_lhds.csv"), col_types = "ccccc") |> 
    mutate(STATEA = str_sub(GISJOIN_PL, 2, 3),
           PLACE = str_sub(GISJOIN_PL, 5, 9))
  
  # if exists, load county lhds  
  if(file.exists(glue("{dat_dir}/tables/lists_to_update/{st}/{st}_county_lhds.csv"))){
    county_lhds <- read_csv(glue("{dat_dir}/tables/lists_to_update/{st}/{st}_county_lhds.csv"), col_types = "ccccc") |> 
      mutate(STATEA = str_sub(GISJOIN_CTY, 2, 3),
             COUNTYA = str_sub(GISJOIN_CTY, 5, 7))
  }
  
  # if exists, load county lhds  
  if(file.exists(glue("{dat_dir}/tables/lists_to_update/{st}/{st}_cousub_lhds.csv"))){
    cousub_lhds <- read_csv(glue("{dat_dir}/tables/lists_to_update/{st}/{st}_cousub_lhds.csv"), col_types = "cccccc") |> 
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
  
  
  ## ---- Create LHD Owner-occupied Value estimates ----
  # st_place070_pl_county has the weights so join the acs_cousub_ownoccvalue df onto the st_place070_pl_county df
  st_place070_pl_county <- st_place070_pl_county |>
    left_join(acs_cousub_ownoccvalue) |>
    mutate(est_less_than_val_10000 = perc_hh * less_than_val_10000,
           est_val_10000_to_val_14999 = perc_hh * val_10000_to_val_14999,
           est_val_15000_to_val_19999 = perc_hh * val_15000_to_val_19999,
           est_val_20000_to_val_24999 = perc_hh * val_20000_to_val_24999,
           est_val_25000_to_val_29999 = perc_hh * val_25000_to_val_29999,
           est_val_30000_to_val_34999 = perc_hh * val_30000_to_val_34999,
           est_val_35000_to_val_39999 = perc_hh * val_35000_to_val_39999,
           est_val_40000_to_val_49999 = perc_hh * val_40000_to_val_49999,
           est_val_50000_to_val_59999 = perc_hh * val_50000_to_val_59999,
           est_val_60000_to_val_69999 = perc_hh * val_60000_to_val_69999,
           est_val_70000_to_val_79999 = perc_hh * val_70000_to_val_79999,
           est_val_80000_to_val_89999 = perc_hh * val_80000_to_val_89999,
           est_val_90000_to_val_99999 = perc_hh * val_90000_to_val_99999,
           est_val_100000_to_val_124999 = perc_hh * val_100000_to_val_124999,
           est_val_125000_to_val_149999 = perc_hh * val_125000_to_val_149999,
           est_val_150000_to_val_174999 = perc_hh * val_150000_to_val_174999,
           est_val_175000_to_val_199999 = perc_hh * val_175000_to_val_199999,
           est_val_200000_to_val_249999 = perc_hh * val_200000_to_val_249999,
           est_val_250000_to_val_299999 = perc_hh * val_250000_to_val_299999,
           est_val_300000_to_val_399999 = perc_hh * val_300000_to_val_399999,
           est_val_400000_to_val_499999 = perc_hh * val_400000_to_val_499999,
           est_val_500000_to_val_749999 = perc_hh * val_500000_to_val_749999,
           est_val_750000_to_val_999999 = perc_hh * val_750000_to_val_999999,
           est_val_1000000_to_val_1499999 = perc_hh * val_1000000_to_val_1499999,
           est_val_1500000_to_val_1999999 = perc_hh * val_1500000_to_val_1999999,
           est_val_2000000_or_more = perc_hh * val_2000000_or_more) 
  
  # generate LHD owner-occupied values estimates based on cousub-weighted values
  st_lhd_ownoccvalue <- st_place070_pl_county |>
    group_by(naccho_id) |>
    summarise(less_than_val_10000 = sum(est_less_than_val_10000),
              val_10000_to_val_14999 = sum(est_val_10000_to_val_14999),
              val_15000_to_val_19999 = sum(est_val_15000_to_val_19999),
              val_20000_to_val_24999 = sum(est_val_20000_to_val_24999),
              val_25000_to_val_29999 = sum(est_val_25000_to_val_29999),
              val_30000_to_val_34999 = sum(est_val_30000_to_val_34999),
              val_35000_to_val_39999 = sum(est_val_35000_to_val_39999),
              val_40000_to_val_49999 = sum(est_val_40000_to_val_49999),
              val_50000_to_val_59999 = sum(est_val_50000_to_val_59999),
              val_60000_to_val_69999 = sum(est_val_60000_to_val_69999),
              val_70000_to_val_79999 = sum(est_val_70000_to_val_79999),
              val_80000_to_val_89999 = sum(est_val_80000_to_val_89999),
              val_90000_to_val_99999 = sum(est_val_90000_to_val_99999),
              val_100000_to_val_124999 = sum(est_val_100000_to_val_124999),
              val_125000_to_val_149999 = sum(est_val_125000_to_val_149999),
              val_150000_to_val_174999 = sum(est_val_150000_to_val_174999),
              val_175000_to_val_199999 = sum(est_val_175000_to_val_199999),
              val_200000_to_val_249999 = sum(est_val_200000_to_val_249999),
              val_250000_to_val_299999 = sum(est_val_250000_to_val_299999),
              val_300000_to_val_399999 = sum(est_val_300000_to_val_399999),
              val_400000_to_val_499999 = sum(est_val_400000_to_val_499999),
              val_500000_to_val_749999 = sum(est_val_500000_to_val_749999),
              val_750000_to_val_999999 = sum(est_val_750000_to_val_999999),
              val_1000000_to_val_1499999 = sum(est_val_1000000_to_val_1499999),
              val_1500000_to_val_1999999 = sum(est_val_1500000_to_val_1999999),
              val_2000000_or_more = sum(est_val_2000000_or_more))
  
  ## ---- Create household income counts for place-based LHDs ----
  # collapse multi-place lhds first
  if(exists("pl_lhds")){
    pl_lhds_ownoccvalue <- pl_lhds |>
      left_join(acs_place_ownoccvalue, by = c("STATEA" = "STATEA", "PLACE" = "PLACEA")) |>
      group_by(naccho_id) |>
      summarise(less_than_val_10000 = sum(less_than_val_10000),
                val_10000_to_val_14999 = sum(val_10000_to_val_14999),
                val_15000_to_val_19999 = sum(val_15000_to_val_19999),
                val_20000_to_val_24999 = sum(val_20000_to_val_24999),
                val_25000_to_val_29999 = sum(val_25000_to_val_29999),
                val_30000_to_val_34999 = sum(val_30000_to_val_34999),
                val_35000_to_val_39999 = sum(val_35000_to_val_39999),
                val_40000_to_val_49999 = sum(val_40000_to_val_49999),
                val_50000_to_val_59999 = sum(val_50000_to_val_59999),
                val_60000_to_val_69999 = sum(val_60000_to_val_69999),
                val_70000_to_val_79999 = sum(val_70000_to_val_79999),
                val_80000_to_val_89999 = sum(val_80000_to_val_89999),
                val_90000_to_val_99999 = sum(val_90000_to_val_99999),
                val_100000_to_val_124999 = sum(val_100000_to_val_124999),
                val_125000_to_val_149999 = sum(val_125000_to_val_149999),
                val_150000_to_val_174999 = sum(val_150000_to_val_174999),
                val_175000_to_val_199999 = sum(val_175000_to_val_199999),
                val_200000_to_val_249999 = sum(val_200000_to_val_249999),
                val_250000_to_val_299999 = sum(val_250000_to_val_299999),
                val_300000_to_val_399999 = sum(val_300000_to_val_399999),
                val_400000_to_val_499999 = sum(val_400000_to_val_499999),
                val_500000_to_val_749999 = sum(val_500000_to_val_749999),
                val_750000_to_val_999999 = sum(val_750000_to_val_999999),
                val_1000000_to_val_1499999 = sum(val_1000000_to_val_1499999),
                val_1500000_to_val_1999999 = sum(val_1500000_to_val_1999999),
                val_2000000_or_more = sum(val_2000000_or_more)) |> 
      filter(!is.na(naccho_id))
    
    
    ## ---- Replace estimates in st_lhc_ownoccvalue by values in pl_lhds_sex ----
    st_lhd_ownoccvalue_drop <- st_lhd_ownoccvalue |>
      filter(!naccho_id %in% pl_lhds_ownoccvalue$naccho_id)
    
    # bind place-based LHDs with remainder
    st_lhd_ownoccvalue_bind <- bind_rows(st_lhd_ownoccvalue_drop, pl_lhds_ownoccvalue) |>
      arrange(naccho_id)
  } else {
    st_lhd_ownoccvalue_bind <- st_lhd_ownoccvalue |> 
      arrange(naccho_id)
  }
  
  ## ---- Create household owner occupied value totals for Ohio
  st_state_ownoccvalue <- acs_cousub_ownoccvalue |>
    filter(STUSAB == st) |>
    summarise(less_than_val_10000 = sum(less_than_val_10000),
              val_10000_to_val_14999 = sum(val_10000_to_val_14999),
              val_15000_to_val_19999 = sum(val_15000_to_val_19999),
              val_20000_to_val_24999 = sum(val_20000_to_val_24999),
              val_25000_to_val_29999 = sum(val_25000_to_val_29999),
              val_30000_to_val_34999 = sum(val_30000_to_val_34999),
              val_35000_to_val_39999 = sum(val_35000_to_val_39999),
              val_40000_to_val_49999 = sum(val_40000_to_val_49999),
              val_50000_to_val_59999 = sum(val_50000_to_val_59999),
              val_60000_to_val_69999 = sum(val_60000_to_val_69999),
              val_70000_to_val_79999 = sum(val_70000_to_val_79999),
              val_80000_to_val_89999 = sum(val_80000_to_val_89999),
              val_90000_to_val_99999 = sum(val_90000_to_val_99999),
              val_100000_to_val_124999 = sum(val_100000_to_val_124999),
              val_125000_to_val_149999 = sum(val_125000_to_val_149999),
              val_150000_to_val_174999 = sum(val_150000_to_val_174999),
              val_175000_to_val_199999 = sum(val_175000_to_val_199999),
              val_200000_to_val_249999 = sum(val_200000_to_val_249999),
              val_250000_to_val_299999 = sum(val_250000_to_val_299999),
              val_300000_to_val_399999 = sum(val_300000_to_val_399999),
              val_400000_to_val_499999 = sum(val_400000_to_val_499999),
              val_500000_to_val_749999 = sum(val_500000_to_val_749999),
              val_750000_to_val_999999 = sum(val_750000_to_val_999999),
              val_1000000_to_val_1499999 = sum(val_1000000_to_val_1499999),
              val_1500000_to_val_1999999 = sum(val_1500000_to_val_1999999),
              val_2000000_or_more = sum(val_2000000_or_more)) |>
    pivot_longer(cols = less_than_val_10000:val_2000000_or_more, values_to = "v1", names_to = "value")
  
  ## ---- IPF for owner-occupied value estimates ----
  ## row = st_ipf_tenure (owner-occupied housing unit counts for each LHD)
  ## seed = st_lhd_ownoccvalue_bind (owner-occupied value estimates for each LHDs)
  ## col = st_state_ownoccvalue
  ## 
  ## The rounded value for the LHD-level owner-occupied count does not sum correctly to the 
  ## state-level owner-occupied count for Ohio (at least). Thus, the ipf2 function will
  ## not run. 
  
  seed <- st_lhd_ownoccvalue_bind |>
    select(less_than_val_10000:val_2000000_or_more)
  rowc <- as.matrix(ipf_tenure$hh_own)
  colc <- as.matrix(st_state_ownoccvalue$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  if(sum(rowc) == sum(colc)){
    st_ipf_ownoccvalue <- ipf2(rowc, colc, seed)$fitted.table
    st_ipf_ownoccvalue <- bind_cols(select(st_lhd_ownoccvalue_bind, naccho_id), st_ipf_ownoccvalue) |> 
      mutate(ipf_ownoccvalue = 1,
             STUSAB = st)
  } else{
    st_ipf_ownoccvalue <- bind_cols(select(st_lhd_ownoccvalue_bind, naccho_id), seed) |> 
      mutate(ipf_ownoccvalue = 0,
             STUSAB = st)
  }
  
  # Remove extraneous df
  if(exists("pl_lhds")){
    rm(pl_lhds)
  }
  
  ## Return st_ipf_ownoccvalue
  return(st_ipf_ownoccvalue)
  
}

## ---- Iterate over all states to generate ACS owner-occupied value counts ----
acs_ownoccvalue_by_state <- map(state_abbr, ~generate_acs_ownoccvalue_ipf(.x))

## ---- Convert out to a tibble using bind_rows ----
acs_ownoccvalue <- bind_rows(acs_ownoccvalue_by_state) 

## ---- Drop NA records from acs_ownoccvalue before writing out to CSV ----
acs_ownoccvalue <- acs_ownoccvalue |> 
  filter(!is.na(naccho_id))

## ---- Create nationwide output file for owner-occupied housing unit value ----
write_csv(acs_ownoccvalue, glue("{acs_output_dir}/{lhd_vintage}_ownoccvalue.csv"))
