# urban_rural_lhd_workflow.R
# Urban - rural population workflow 
# 
# This script creates urban and rural counts (from the 2020 Decennial census) for each LHD, 
# 
# This script loads in 2020 census block counts (from NHGIS) and the urban/rural census block assignments
# from MABLE/Geocorr. It assigns census blocks to LHDs and computes the 2020 urban and rural pop counts
# for each LHDs.
# 
# Washington DC is 100% urban, so I add special handling for it.

require(tidyverse)
require(glue)
require(vroom)

options(scipen = 999)

## ---- Load geocorr crosswalk between 2020 census blocks and urban/rural status ----
g1 <- vroom(file_geocorr1, col_types = "cccccii")
g2 <- vroom(file_geocorr2, col_types = "cccccii")
g3 <- vroom(file_geocorr3, col_types = "cccccii")
g4 <- vroom(file_geocorr4, col_types = "cccccii")

## ---- Create a nationwide data frame from g1 - g4) ----
ur <- bind_rows(list(g1, g2, g3, g4))

## ---- Remove the g1-g4 data frames to same memory ----
rm(g1)
rm(g2)
rm(g3)
rm(g4)

## ---- Prepare the ur df to join with blk data frame ----
ur <- ur |>
  mutate(tct = str_remove(tract, "\\."),
         GEOCODE = paste0(county, tct, block)) |>
  select(GEOCODE, ur, afact, pop20)

## ---- Load 2020 decennial census block data ----
blk <- vroom(file_blkpop20)

## ---- Retain only key fields from 2020 decennial block data frame ----
blk <- blk |>
  select(GEOCODE, STUSAB, STATEA, COUNTYA, COUSUBA, PLACEA, TRACTA, BLOCKA, blkpop20 = U7B001) 

## ---- Join the UR onto the 2020 block file ----
blk <- blk |>
  left_join(ur, by = "GEOCODE")

## ---- Load in shapefiles for a state ----

generate_ur_counts_by_state <- function(st){

  print(st)
  
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
  
  ## ---- Create subset of blk df for a single state ----
  st_blk <- blk |>
    filter(STUSAB == st)
  
  ## --- Assigns LHDs to records in Place070 ----
  
  ## Join place LHDs onto st_blk data frame
  st_blk_pl <- st_blk |> 
    left_join(pl_lhds, by = c("STATEA" = "STATEA", "PLACEA" = "PLACE")) |> 
    rename(naccho_id_place = naccho_id)
  
  ## Join the county LHD onto the st_blk_pl_county data frame if the county LHD exists
  if(exists("county_lhds")){
    st_blk_pl_county <- st_blk_pl |> 
      left_join(county_lhds, by = c("STATEA" = "STATEA", "COUNTYA" = "COUNTYA")) |> 
      rename(naccho_id_county = naccho_id)
  } else {
    st_blk_pl_county <- st_blk_pl |> 
      mutate(naccho_id_county = NA_character_)
  }
  
  ## Join the county subdivision-based LHDs onto st_blk_pl_county if the county subdivision LHD object exists. If 
  ## it doesn't exist, add a naccho_id_county = NA_character_ variable to the st_blk_pl_county dataframe
  if(exists("cousub_lhds")){
    st_blk_pl_county <- st_blk_pl_county |> 
      left_join(cousub_lhds, by = c("STATEA" = "STATEA", "COUNTYA" = "COUNTYA", "COUSUBA" = "COUSUB"))  |> 
      rename(naccho_id_cousub = naccho_id)
  } else {
    st_blk_pl_county <- st_blk_pl_county |>
      mutate(naccho_id_cousub = NA_character_)
  }
  
  ## Create a new naccho_id keeping place over county and cousub
  st_blk_pl_county <- st_blk_pl_county |> 
    mutate(naccho_id = case_when(!is.na(naccho_id_place) ~ naccho_id_place,
                                 !is.na(naccho_id_cousub) ~ naccho_id_cousub,
                                 !is.na(naccho_id_county) ~ naccho_id_county))
  
  ## ---- Create LHD counts of urban/rural ----
  if(st == "DC"){
    st_lhd_ur <- st_blk_pl_county |> 
      group_by(naccho_id, ur) |> 
      summarise(pop20 = sum(blkpop20)) |> 
      pivot_wider(names_from = ur, values_from = pop20, values_fill = 0) |>
      rename(urban20 = U) |>
      mutate(rural20 = 0,
             pop20 = urban20 + rural20,
             prop_urban20 = urban20 / pop20) |> 
      filter(!is.na(naccho_id))
  } else {
    st_lhd_ur <- st_blk_pl_county |> 
      group_by(naccho_id, ur) |> 
      summarise(pop20 = sum(blkpop20)) |> 
      pivot_wider(names_from = ur, values_from = pop20, values_fill = 0) |>
      rename(urban20 = U,
             rural20 = R) |>
      mutate(pop20 = urban20 + rural20,
             prop_urban20 = urban20 / pop20) |> 
      filter(!is.na(naccho_id))
  }
  
  st_lhd_ur <- st_lhd_ur |> 
    mutate(STUSAB = st)

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
  
  return(st_lhd_ur)
}

out <- map(state_abbr, ~generate_ur_counts_by_state(.x))

## ---- Convert out to a tibble using bind_rows ----
out_tbl <- bind_rows(out)

## ---- Drop the records with naccho_id = NA and write out the new data frame to CSVs
out_tbl <- out_tbl |> 
  filter(!is.na(naccho_id))

## ---- Write out a nationwide LHD urban/rural CSV file ----
write_csv(out_tbl, glue("{ur_output_dir}/{lhd_vintage}_urban_rural_2020.csv"))
