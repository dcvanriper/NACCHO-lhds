# ipf_hhincome.R
# This script attempts to generate LHD estimates of binned household income
# using iterative proportional fitting.

require(tidyverse)
require(vroom)
require(glue)
require(ipumsr) 

# If the directory in the data object doesn't exist, the next code snipped will create the directory
if(!dir.exists(download_acs_hhincome_dir)){
  dir.create(download_acs_hhincome_dir, recursive= TRUE)
}

# Define and submit extract
if(length(list.files(download_acs_hhincome_dir)) == 0){
  define_extract_nhgis(
    description = glue("County Subdivision & Place Household Income - {acs_dataset}"),
    datasets = ds_spec(acs_dataset,
                       data_tables = acs_hhincome,
                       geog_levels = c("cty_sub", "place"))) |>
    submit_extract() |>
    wait_for_extract() |>
    download_extract(download_acs_hhincome_dir)
}

## ---- Load the ACS extract containing the tenure data ----
file <- list.files(download_acs_hhincome_dir, "zip", full.names = TRUE)

acs_cousub <- read_nhgis(file, file_select = contains("cty_"))
acs_place <- read_nhgis(file, file_select = contains("place"))

# Get table-metadata for the four Place070 tables
meta <- get_metadata_nhgis(dataset = acs_dataset, data_table = acs_hhincome)

# Get variables for sex by age table
meta_variables <- meta$variables

# Create descriptive vars from descriptoin and add E to the nhgis_codes
meta_variables <- meta_variables |>
  mutate(nhgis_code = paste0(str_sub(nhgis_code, 1,4), "E", str_sub(nhgis_code, 5,8)),
         description = str_remove(description, ":"),
         description = str_replace_all(description, "[ ]", "_"),
         description = str_replace_all(description, "[,]", ""),
         description = str_replace_all(description, "[$]", "hhinc_"),
         description = tolower(description))

# Create vectors for the rename_with function
recode_key <- purrr::set_names(c(meta_variables$description), c(meta_variables$nhgis_code))

# Rename columns to generic names
acs_cousub <- acs_cousub |>
  rename_with(~recode(colnames(acs_cousub), !!!recode_key))

acs_place <- acs_place |>
  rename_with(~recode(colnames(acs_place), !!!recode_key))

# Retain required fields for sex
acs_cousub_hhincome <- acs_cousub |>
  select(STUSAB, STATEA, COUNTYA, COUSUBA, less_than_hhinc_10000:hhinc_200000_or_more)

acs_place_hhincome <- acs_place |>
  select(STUSAB, STATEA, PLACEA, less_than_hhinc_10000:hhinc_200000_or_more)

## ---- Function to generate ACS household income IPF estimates by state ----
generate_acs_hhincome_ipf <- function(st){
  
  print(st)
  
  # Filter acs_totals (by LHD) to a single state
  st_lhd_ACS <- acs_totals |> 
    filter(STUSAB == st)
  
  # Filter place070 to a single state
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
  
  ## ---- Create LHD Household Income estimates ----
  # st_place070_cousub_pl has the weights so join the acs_cousub_hhincome df onto the st_place070_cousub_pl df
  st_place070_pl_county <- st_place070_pl_county |> 
    left_join(acs_cousub_hhincome) |> 
    mutate(est_less_than_hhinc_10000 = perc_hh * less_than_hhinc_10000,
           est_hhinc_10000_to_hhinc_14999 = perc_hh * hhinc_10000_to_hhinc_14999,
           est_hhinc_15000_to_hhinc_19999 = perc_hh * hhinc_15000_to_hhinc_19999,
           est_hhinc_20000_to_hhinc_24999 = perc_hh * hhinc_20000_to_hhinc_24999,
           est_hhinc_25000_to_hhinc_29999 = perc_hh * hhinc_25000_to_hhinc_29999,
           est_hhinc_30000_to_hhinc_34999 = perc_hh * hhinc_30000_to_hhinc_34999,
           est_hhinc_35000_to_hhinc_39999 = perc_hh * hhinc_35000_to_hhinc_39999,
           est_hhinc_40000_to_hhinc_44999 = perc_hh * hhinc_40000_to_hhinc_44999,
           est_hhinc_45000_to_hhinc_49999 = perc_hh * hhinc_45000_to_hhinc_49999,
           est_hhinc_50000_to_hhinc_59999 = perc_hh * hhinc_50000_to_hhinc_59999,
           est_hhinc_60000_to_hhinc_74999 = perc_hh * hhinc_60000_to_hhinc_74999,
           est_hhinc_75000_to_hhinc_99999 = perc_hh * hhinc_75000_to_hhinc_99999,
           est_hhinc_100000_to_hhinc_124999 = perc_hh * hhinc_100000_to_hhinc_124999,
           est_hhinc_125000_to_hhinc_149999 = perc_hh * hhinc_125000_to_hhinc_149999,
           est_hhinc_150000_to_hhinc_199999 = perc_hh * hhinc_150000_to_hhinc_199999,
           est_hhinc_200000_or_more = perc_hh * hhinc_200000_or_more) 
  
  # generate LHD household income estimates based on cousub-weighted values
  st_lhd_hhincome <- st_place070_pl_county |> 
    group_by(naccho_id) |> 
    summarise(less_than_hhinc_10000 = sum(est_less_than_hhinc_10000),
              hhinc_10000_to_hhinc_14999 = sum(est_hhinc_10000_to_hhinc_14999),
              hhinc_15000_to_hhinc_19999 = sum(est_hhinc_15000_to_hhinc_19999),
              hhinc_20000_to_hhinc_24999 = sum(est_hhinc_20000_to_hhinc_24999),
              hhinc_25000_to_hhinc_29999 = sum(est_hhinc_25000_to_hhinc_29999),
              hhinc_30000_to_hhinc_34999 = sum(est_hhinc_30000_to_hhinc_34999),
              hhinc_35000_to_hhinc_39999 = sum(est_hhinc_35000_to_hhinc_39999),
              hhinc_40000_to_hhinc_44999 = sum(est_hhinc_40000_to_hhinc_44999),
              hhinc_45000_to_hhinc_49999 = sum(est_hhinc_45000_to_hhinc_49999),
              hhinc_50000_to_hhinc_59999 = sum(est_hhinc_50000_to_hhinc_59999),
              hhinc_60000_to_hhinc_74999 = sum(est_hhinc_60000_to_hhinc_74999),
              hhinc_75000_to_hhinc_99999 = sum(est_hhinc_75000_to_hhinc_99999),
              hhinc_100000_to_hhinc_124999 = sum(est_hhinc_100000_to_hhinc_124999),
              hhinc_125000_to_hhinc_149999 = sum(est_hhinc_125000_to_hhinc_149999),
              hhinc_150000_to_hhinc_199999 = sum(est_hhinc_150000_to_hhinc_199999),
              hhinc_200000_or_more = sum(est_hhinc_200000_or_more))
  
  ## ---- Create household income counts for place-based LHDs ----
  # collapse multi-place lhds first
  if(exists("pl_lhds")){
    pl_lhds_hhincome <- pl_lhds |> 
      left_join(acs_place_hhincome, by = c("STATEA" = "STATEA", "PLACE" = "PLACEA")) |> 
      group_by(naccho_id) |> 
      summarise(less_than_hhinc_10000 = sum(less_than_hhinc_10000),
                hhinc_10000_to_hhinc_14999 = sum(hhinc_10000_to_hhinc_14999),
                hhinc_15000_to_hhinc_19999 = sum(hhinc_15000_to_hhinc_19999),
                hhinc_20000_to_hhinc_24999 = sum(hhinc_20000_to_hhinc_24999),
                hhinc_25000_to_hhinc_29999 = sum(hhinc_25000_to_hhinc_29999),
                hhinc_30000_to_hhinc_34999 = sum(hhinc_30000_to_hhinc_34999),
                hhinc_35000_to_hhinc_39999 = sum(hhinc_35000_to_hhinc_39999),
                hhinc_40000_to_hhinc_44999 = sum(hhinc_40000_to_hhinc_44999),
                hhinc_45000_to_hhinc_49999 = sum(hhinc_45000_to_hhinc_49999),
                hhinc_50000_to_hhinc_59999 = sum(hhinc_50000_to_hhinc_59999),
                hhinc_60000_to_hhinc_74999 = sum(hhinc_60000_to_hhinc_74999),
                hhinc_75000_to_hhinc_99999 = sum(hhinc_75000_to_hhinc_99999),
                hhinc_100000_to_hhinc_124999 = sum(hhinc_100000_to_hhinc_124999),
                hhinc_125000_to_hhinc_149999 = sum(hhinc_125000_to_hhinc_149999),
                hhinc_150000_to_hhinc_199999 = sum(hhinc_150000_to_hhinc_199999),
                hhinc_200000_or_more = sum(hhinc_200000_or_more)) |> 
      filter(!is.na(naccho_id))
    
  
  ## ---- Replace estimates in oh_lhd_sex by values in pl_lhds_sex ----
  st_lhd_hhincome_drop <- st_lhd_hhincome |> 
    filter(!naccho_id %in% pl_lhds_hhincome$naccho_id)
  
  # bind place-based LHDs with remainder
  st_lhd_hhincome_bind <- bind_rows(st_lhd_hhincome_drop, pl_lhds_hhincome) |> 
    arrange(naccho_id)
  
  } else{
    st_lhd_hhincome_bind <- st_lhd_hhincome |> 
      arrange(naccho_id)
  }
  
  ## ---- Create household income totals for Ohio
  st_state_hhincome <- acs_cousub_hhincome |> 
    filter(STUSAB == st) |> 
    summarise(less_than_hhinc_10000 = sum(less_than_hhinc_10000),
              hhinc_10000_to_hhinc_14999 = sum(hhinc_10000_to_hhinc_14999),
              hhinc_15000_to_hhinc_19999 = sum(hhinc_15000_to_hhinc_19999),
              hhinc_20000_to_hhinc_24999 = sum(hhinc_20000_to_hhinc_24999),
              hhinc_25000_to_hhinc_29999 = sum(hhinc_25000_to_hhinc_29999),
              hhinc_30000_to_hhinc_34999 = sum(hhinc_30000_to_hhinc_34999),
              hhinc_35000_to_hhinc_39999 = sum(hhinc_35000_to_hhinc_39999),
              hhinc_40000_to_hhinc_44999 = sum(hhinc_40000_to_hhinc_44999),
              hhinc_45000_to_hhinc_49999 = sum(hhinc_45000_to_hhinc_49999),
              hhinc_50000_to_hhinc_59999 = sum(hhinc_50000_to_hhinc_59999),
              hhinc_60000_to_hhinc_74999 = sum(hhinc_60000_to_hhinc_74999),
              hhinc_75000_to_hhinc_99999 = sum(hhinc_75000_to_hhinc_99999),
              hhinc_100000_to_hhinc_124999 = sum(hhinc_100000_to_hhinc_124999),
              hhinc_125000_to_hhinc_149999 = sum(hhinc_125000_to_hhinc_149999),
              hhinc_150000_to_hhinc_199999 = sum(hhinc_150000_to_hhinc_199999),
              hhinc_200000_or_more = sum(hhinc_200000_or_more)) |> 
    pivot_longer(cols = less_than_hhinc_10000:hhinc_200000_or_more, values_to = "v1", names_to = "hhincome")
  
  ## ---- IPF for household income estimates ----
  ## row = st_lhd_ACS (total households each LHD)
  ## seed = st_lhd_income_bind (tenure estimates for each LHDs)
  ## col = st_state_tenure
  
  seed <- st_lhd_hhincome_bind |> 
    select(less_than_hhinc_10000:hhinc_200000_or_more)
  rowc <- as.matrix(st_lhd_ACS$hh)
  colc <- as.matrix(st_state_hhincome$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  if(sum(rowc) == sum(colc)){
    st_ipf_hhincome <- ipf2(rowc, colc, seed)$fitted.table
    st_ipf_hhincome <- bind_cols(select(st_lhd_hhincome_bind, naccho_id), st_ipf_hhincome) |> 
      mutate(ipf_hhincome = 1,
             STUSAB = st)
  } else {
    st_ipf_hhincome <- bind_cols(select(st_lhd_hhincome_bind, naccho_id), seed) |> 
      mutate(ipf_hhincome = 0,
             STUSAB = st)
  }
  
  # Remove extraneous df
  if(exists("pl_lhds")){
    rm(pl_lhds)
  }
  
  ## Return st_ipf_hhincome
  return(st_ipf_hhincome)
}

## ---- Iterate over all states to generate ACS household income counts ----
acs_hhincome_by_state <- map(state_abbr, ~generate_acs_hhincome_ipf(.x))

## ---- Convert out to a tibble using bind_rows ----
acs_hhincome <- bind_rows(acs_hhincome_by_state) 

## ---- Drop NA records from acs_hhincome before writing out to CSV ----
acs_hhincome <- acs_hhincome |> 
  filter(!is.na(naccho_id))

## ---- Create nationwide output file for household income ----
write_csv(acs_hhincome, glue("{acs_output_dir}/{lhd_vintage}_hhincome.csv"))
