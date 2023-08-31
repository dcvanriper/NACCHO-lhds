# ipf_sex_sexage.R
# This script generates LHD estimates of males and females using iterative
# proportional fitting. It then generates LHD estimates of females by age and
# males by age using iterative proportional fitting.

require(tidyverse)
require(vroom)
require(glue)
require(ipumsr)

## ---- Create and download an ACS 5-year extract for the sex IPF ----
# If the directory in the data object doesn't exist, the next code snipped will create the directory
if(!dir.exists(download_acs_sexage_dir)){
  dir.create(download_acs_sexage_dir, recursive= TRUE)
}

# Define and submit extract
if(length(list.files(download_acs_sexage_dir)) == 0){
  define_extract_nhgis(
    description = glue("County Subdivision & Place Sex by Age - {acs_dataset}"),
    datasets = ds_spec(acs_dataset,
    data_tables = acs_sexage,
    geog_levels = c("cty_sub", "place")
    )) |>
    submit_extract() |>
    wait_for_extract() |>
    download_extract(download_acs_sexage_dir)
}

## ---- Load the ACS extract containing the sex by age data ----
file <- list.files(download_acs_sexage_dir, "zip", full.names = TRUE)

acs_cousub <- read_nhgis(file, file_select = contains("cty_"))
acs_place <- read_nhgis(file, file_select = contains("place"))

# Get table-metadata for the four Place070 tables
meta <- get_metadata_nhgis(dataset = acs_dataset, data_table = acs_sexage)

# Get variables for sex by age table
meta_variables <- meta$variables

# Create descriptive vars from description and add E to the nhgis_codes
meta_variables <- meta_variables |>
  mutate(nhgis_code = paste0(str_sub(nhgis_code, 1,4), "E", str_sub(nhgis_code, 5,8)),
         description = str_remove(description, ":"),
         description = str_replace_all(description, "[ ]", "_"),
         description = tolower(description))

# Create vectors for the rename_with function
recode_key <- purrr::set_names(c(meta_variables$description), c(meta_variables$nhgis_code))

# Rename columns to generic names
acs_cousub <- acs_cousub |>
  rename_with(~recode(colnames(acs_cousub), !!!recode_key))

acs_place <- acs_place |>
  rename_with(~recode(colnames(acs_place), !!!recode_key))

# Retain required fields for sex
acs_cousub_sex <- acs_cousub |>
  select(STUSAB, STATEA, COUNTYA, COUSUBA, male, female)

acs_place_sex <- acs_place |>
  select(STUSAB, STATEA, PLACEA, male, female)

# Retain required fields for sex by age
acs_cousub_sexage <- acs_cousub |>
  select(STUSAB, STATEA, COUNTYA, COUSUBA, starts_with("male_"), starts_with("female_"))

acs_place_sexage <- acs_place |>
  select(STUSAB, STATEA, PLACEA, starts_with("male_"), starts_with("female_"))

## ---- Function to generate ACS sex IPF estimates by state ----
generate_acs_sex_ipf <- function(st){
  
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
  
  
  ## Create LHD Sex estimates
  ## st_place070_pl_county has the weights so join the coubsub_sex df onto the st_place070_pl_county df
  st_place070_pl_county <- st_place070_pl_county  |> 
    left_join(acs_cousub_sex)  |> 
    mutate(est_male = perc_totpop * male,
           est_female = perc_totpop * female) 
  
  ## Generate LHD sex estimates based on cousub-weighted values
  st_lhd_sex <- st_place070_pl_county |> 
    group_by(naccho_id)  |> 
    summarise(male = sum(est_male),
              female = sum(est_female))
  
  ## Create male and female counts for place-based LHDs
  # collapse multi-place lhds first
  if(exists("pl_lhds")){
    pl_lhds_sex <- pl_lhds  |> 
      left_join(acs_place_sex, by = c("STATEA" = "STATEA", "PLACE" = "PLACEA")) |>
      group_by(naccho_id) |>
      summarise(male = sum(male),
                female = sum(female)) |> 
      filter(!is.na(naccho_id))
    
    ## Replace estimates in st_lhd_sex by values in pl_lhds_sex
    ## This breaks -- since all places are in file with naccho_id == NA, then I get a record with all males and
    ## females in the state, which I don't want
    st_lhd_sex_drop <- st_lhd_sex |>
      filter(!naccho_id %in% pl_lhds_sex$naccho_id)
    
    # bind place-based LHDs with remainder
    st_lhd_sex_bind <- bind_rows(st_lhd_sex_drop, pl_lhds_sex) |>
      #filter(!is.na(naccho_id) & !is.na(male) & !is.na(female)) |>
      arrange(naccho_id)
    }else{
      st_lhd_sex_bind <- st_lhd_sex |>
        #filter(!is.na(naccho_id) & !is.na(male) & !is.na(female)) |>
        arrange(naccho_id)
    }
  
  ## Create male and female totals for a single state
  st_state_sex <- acs_cousub_sex |>
    filter(STUSAB == st) |> 
    summarise(male = sum(male),
              female = sum(female)) |>
    pivot_longer(cols = male:female, values_to = "v1", names_to = "sex")
  
  ## ---- IPF for sex estimates ----
  ## row = st_lhd_ACS (total pop for each LHD)
  ## seed = st_lhd_sex_bind (male and female estimates for each LHDs)
  ## col = st_state_sex
  seed <- st_lhd_sex_bind |>
    select(male, female)
  rowc <- as.matrix(st_lhd_ACS$totpop)
  colc <- as.matrix(st_state_sex$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  st_ipf_sex <- ipf2(rowc, colc, seed)$fitted.table
  
  ## Join the naccho_ids onto the IPF sex counts and round counts
  ## IPF will work, by definition, for sex counts
  st_ipf_sex <- bind_cols(select(st_lhd_sex_bind, naccho_id), st_ipf_sex) |>
    mutate(male = round(male, digits = 0),
           female = round(female, digits = 0),
           ipf_sex = 1,
           STUSAB = st)
  
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
  
  return(st_ipf_sex)
}
  
## ---- Iterate over all states to generate ACS sex counts ----
acs_sex_by_state <- map(state_abbr, ~generate_acs_sex_ipf(.x))

## ---- Convert out to a tibble using bind_rows ----
acs_sex <- bind_rows(acs_sex_by_state) 

## ---- IPF for sex by age ----
## Running the sex by age IPF requires the sex counts generated in the first part of this script.

## ---- Function to generate ACS sex by age IPF estimates by state ----
generate_acs_sexage_ipf <- function(st){
  
  # Filter acs_totals (by LHD) to a single state
  st_lhd_ACS <- acs_totals |> 
    filter(STUSAB == st)
  
  # Filter the ipf_sex estimates (by LHD) to a single state
  ipf_sex <- acs_sex |> 
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
  
  
  ## Create sex by age estimates
  # st_place070_cousub_pl has the weights so join the coubsub_sexage df onto the st_place070_cousub_pl df
  st_place070_pl_county <- st_place070_pl_county |>
    left_join(acs_cousub_sexage) |>
    mutate(est_male_under_5_years = perc_totpop * male_under_5_years,
           est_male_5_to_9_years = perc_totpop * male_5_to_9_years,
           est_male_10_to_14_years = perc_totpop * male_10_to_14_years,
           est_male_15_to_17_years = perc_totpop * male_15_to_17_years,
           est_male_18_and_19_years = perc_totpop * male_18_and_19_years,
           est_male_20_years = perc_totpop * male_20_years,
           est_male_21_years = perc_totpop * male_21_years,
           est_male_22_to_24_years = perc_totpop * male_22_to_24_years,
           est_male_25_to_29_years = perc_totpop * male_25_to_29_years,
           est_male_30_to_34_years = perc_totpop * male_30_to_34_years,
           est_male_35_to_39_years = perc_totpop * male_35_to_39_years,
           est_male_40_to_44_years = perc_totpop * male_40_to_44_years,
           est_male_45_to_49_years = perc_totpop * male_45_to_49_years,
           est_male_50_to_54_years = perc_totpop * male_50_to_54_years,
           est_male_55_to_59_years = perc_totpop * male_55_to_59_years,
           est_male_60_and_61_years = perc_totpop * male_60_and_61_years,
           est_male_62_to_64_years = perc_totpop * male_62_to_64_years,
           est_male_65_and_66_years = perc_totpop * male_65_and_66_years,
           est_male_67_to_69_years = perc_totpop * male_67_to_69_years,
           est_male_70_to_74_years = perc_totpop * male_70_to_74_years,
           est_male_75_to_79_years = perc_totpop * male_75_to_79_years,
           est_male_80_to_84_years = perc_totpop * male_80_to_84_years,
           est_male_85_years_and_over = perc_totpop * male_85_years_and_over,
           est_female_under_5_years = perc_totpop * female_under_5_years,
           est_female_5_to_9_years = perc_totpop * female_5_to_9_years,
           est_female_10_to_14_years = perc_totpop * female_10_to_14_years,
           est_female_15_to_17_years = perc_totpop * female_15_to_17_years,
           est_female_18_and_19_years = perc_totpop * female_18_and_19_years,
           est_female_20_years = perc_totpop * female_20_years,
           est_female_21_years = perc_totpop * female_21_years,
           est_female_22_to_24_years = perc_totpop * female_22_to_24_years,
           est_female_25_to_29_years = perc_totpop * female_25_to_29_years,
           est_female_30_to_34_years = perc_totpop * female_30_to_34_years,
           est_female_35_to_39_years = perc_totpop * female_35_to_39_years,
           est_female_40_to_44_years = perc_totpop * female_40_to_44_years,
           est_female_45_to_49_years = perc_totpop * female_45_to_49_years,
           est_female_50_to_54_years = perc_totpop * female_50_to_54_years,
           est_female_55_to_59_years = perc_totpop * female_55_to_59_years,
           est_female_60_and_61_years = perc_totpop * female_60_and_61_years,
           est_female_62_to_64_years = perc_totpop * female_62_to_64_years,
           est_female_65_and_66_years = perc_totpop * female_65_and_66_years,
           est_female_67_to_69_years = perc_totpop * female_67_to_69_years,
           est_female_70_to_74_years = perc_totpop * female_70_to_74_years,
           est_female_75_to_79_years = perc_totpop * female_75_to_79_years,
           est_female_80_to_84_years = perc_totpop * female_80_to_84_years,
           est_female_85_years_and_over = perc_totpop * female_85_years_and_over) 
  
  ## Generate LHD sex estimates based on cousub-weighted values
  st_lhd_sexage <- st_place070_pl_county |>
    group_by(naccho_id) |>
    summarise(male_under_5_years = sum(est_male_under_5_years),
              male_5_to_9_years = sum(est_male_5_to_9_years),
              male_10_to_14_years = sum(est_male_10_to_14_years),
              male_15_to_17_years = sum(est_male_15_to_17_years),
              male_18_and_19_years = sum(est_male_18_and_19_years),
              male_20_years = sum(est_male_20_years),
              male_21_years = sum(est_male_21_years),
              male_22_to_24_years = sum(est_male_22_to_24_years),
              male_25_to_29_years = sum(est_male_25_to_29_years),
              male_30_to_34_years = sum(est_male_30_to_34_years),
              male_35_to_39_years = sum(est_male_35_to_39_years),
              male_40_to_44_years = sum(est_male_40_to_44_years),
              male_45_to_49_years = sum(est_male_45_to_49_years),
              male_50_to_54_years = sum(est_male_50_to_54_years),
              male_55_to_59_years = sum(est_male_55_to_59_years),
              male_60_and_61_years = sum(est_male_60_and_61_years),
              male_62_to_64_years = sum(est_male_62_to_64_years),
              male_65_and_66_years = sum(est_male_65_and_66_years),
              male_67_to_69_years = sum(est_male_67_to_69_years),
              male_70_to_74_years = sum(est_male_70_to_74_years),
              male_75_to_79_years = sum(est_male_75_to_79_years),
              male_80_to_84_years = sum(est_male_80_to_84_years),
              male_85_years_and_over = sum(est_male_85_years_and_over),
              female_under_5_years = sum(est_female_under_5_years),
              female_5_to_9_years = sum(est_female_5_to_9_years),
              female_10_to_14_years = sum(est_female_10_to_14_years),
              female_15_to_17_years = sum(est_female_15_to_17_years),
              female_18_and_19_years = sum(est_female_18_and_19_years),
              female_20_years = sum(est_female_20_years),
              female_21_years = sum(est_female_21_years),
              female_22_to_24_years = sum(est_female_22_to_24_years),
              female_25_to_29_years = sum(est_female_25_to_29_years),
              female_30_to_34_years = sum(est_female_30_to_34_years),
              female_35_to_39_years = sum(est_female_35_to_39_years),
              female_40_to_44_years = sum(est_female_40_to_44_years),
              female_45_to_49_years = sum(est_female_45_to_49_years),
              female_50_to_54_years = sum(est_female_50_to_54_years),
              female_55_to_59_years = sum(est_female_55_to_59_years),
              female_60_and_61_years = sum(est_female_60_and_61_years),
              female_62_to_64_years = sum(est_female_62_to_64_years),
              female_65_and_66_years = sum(est_female_65_and_66_years),
              female_67_to_69_years = sum(est_female_67_to_69_years),
              female_70_to_74_years = sum(est_female_70_to_74_years),
              female_75_to_79_years = sum(est_female_75_to_79_years),
              female_80_to_84_years = sum(est_female_80_to_84_years),
              female_85_years_and_over = sum(est_female_85_years_and_over))
  
  ## Create male and female counts for place-based LHDs
  # collapse multi-place lhds first
  if(exists("pl_lhds")){
    
    pl_lhds_sexage <- pl_lhds |>
      left_join(acs_place_sexage, by = c("STATEA" = "STATEA", "PLACE" = "PLACEA")) |>
      group_by(naccho_id) |>
      summarise(male_under_5_years = sum(male_under_5_years),
                male_5_to_9_years = sum(male_5_to_9_years),
                male_10_to_14_years = sum(male_10_to_14_years),
                male_15_to_17_years = sum(male_15_to_17_years),
                male_18_and_19_years = sum(male_18_and_19_years),
                male_20_years = sum(male_20_years),
                male_21_years = sum(male_21_years),
                male_22_to_24_years = sum(male_22_to_24_years),
                male_25_to_29_years = sum(male_25_to_29_years),
                male_30_to_34_years = sum(male_30_to_34_years),
                male_35_to_39_years = sum(male_35_to_39_years),
                male_40_to_44_years = sum(male_40_to_44_years),
                male_45_to_49_years = sum(male_45_to_49_years),
                male_50_to_54_years = sum(male_50_to_54_years),
                male_55_to_59_years = sum(male_55_to_59_years),
                male_60_and_61_years = sum(male_60_and_61_years),
                male_62_to_64_years = sum(male_62_to_64_years),
                male_65_and_66_years = sum(male_65_and_66_years),
                male_67_to_69_years = sum(male_67_to_69_years),
                male_70_to_74_years = sum(male_70_to_74_years),
                male_75_to_79_years = sum(male_75_to_79_years),
                male_80_to_84_years = sum(male_80_to_84_years),
                male_85_years_and_over = sum(male_85_years_and_over),
                female_under_5_years = sum(female_under_5_years),
                female_5_to_9_years = sum(female_5_to_9_years),
                female_10_to_14_years = sum(female_10_to_14_years),
                female_15_to_17_years = sum(female_15_to_17_years),
                female_18_and_19_years = sum(female_18_and_19_years),
                female_20_years = sum(female_20_years),
                female_21_years = sum(female_21_years),
                female_22_to_24_years = sum(female_22_to_24_years),
                female_25_to_29_years = sum(female_25_to_29_years),
                female_30_to_34_years = sum(female_30_to_34_years),
                female_35_to_39_years = sum(female_35_to_39_years),
                female_40_to_44_years = sum(female_40_to_44_years),
                female_45_to_49_years = sum(female_45_to_49_years),
                female_50_to_54_years = sum(female_50_to_54_years),
                female_55_to_59_years = sum(female_55_to_59_years),
                female_60_and_61_years = sum(female_60_and_61_years),
                female_62_to_64_years = sum(female_62_to_64_years),
                female_65_and_66_years = sum(female_65_and_66_years),
                female_67_to_69_years = sum(female_67_to_69_years),
                female_70_to_74_years = sum(female_70_to_74_years),
                female_75_to_79_years = sum(female_75_to_79_years),
                female_80_to_84_years = sum(female_80_to_84_years),
                female_85_years_and_over = sum(female_85_years_and_over)) |> 
      filter(!is.na(naccho_id))
    
    
    ## ---- Replace estimates in oh_lhd_sexage by values in pl_lhds_sexage ----
    st_lhd_sexage_drop <- st_lhd_sexage |>
      filter(!naccho_id %in% pl_lhds_sexage$naccho_id)
    
    # bind place-based LHDs with remainder
    st_lhd_sexage_bind <- bind_rows(st_lhd_sexage_drop, pl_lhds_sexage) |>
      #filter(!is.na(naccho_id)) |>
      arrange(naccho_id)
    
    
  }else{
    st_lhd_sexage_bind <- st_lhd_sexage |>
      # filter(!is.na(naccho_id)) |>
      arrange(naccho_id)
  }
  
  # Create sex by age totals for the state
  st_state_sexage <- acs_cousub_sexage |>
    filter(STUSAB == st) |>
    summarise(male_under_5_years = sum(male_under_5_years),
              male_5_to_9_years = sum(male_5_to_9_years),
              male_10_to_14_years = sum(male_10_to_14_years),
              male_15_to_17_years = sum(male_15_to_17_years),
              male_18_and_19_years = sum(male_18_and_19_years),
              male_20_years = sum(male_20_years),
              male_21_years = sum(male_21_years),
              male_22_to_24_years = sum(male_22_to_24_years),
              male_25_to_29_years = sum(male_25_to_29_years),
              male_30_to_34_years = sum(male_30_to_34_years),
              male_35_to_39_years = sum(male_35_to_39_years),
              male_40_to_44_years = sum(male_40_to_44_years),
              male_45_to_49_years = sum(male_45_to_49_years),
              male_50_to_54_years = sum(male_50_to_54_years),
              male_55_to_59_years = sum(male_55_to_59_years),
              male_60_and_61_years = sum(male_60_and_61_years),
              male_62_to_64_years = sum(male_62_to_64_years),
              male_65_and_66_years = sum(male_65_and_66_years),
              male_67_to_69_years = sum(male_67_to_69_years),
              male_70_to_74_years = sum(male_70_to_74_years),
              male_75_to_79_years = sum(male_75_to_79_years),
              male_80_to_84_years = sum(male_80_to_84_years),
              male_85_years_and_over = sum(male_85_years_and_over),
              female_under_5_years = sum(female_under_5_years),
              female_5_to_9_years = sum(female_5_to_9_years),
              female_10_to_14_years = sum(female_10_to_14_years),
              female_15_to_17_years = sum(female_15_to_17_years),
              female_18_and_19_years = sum(female_18_and_19_years),
              female_20_years = sum(female_20_years),
              female_21_years = sum(female_21_years),
              female_22_to_24_years = sum(female_22_to_24_years),
              female_25_to_29_years = sum(female_25_to_29_years),
              female_30_to_34_years = sum(female_30_to_34_years),
              female_35_to_39_years = sum(female_35_to_39_years),
              female_40_to_44_years = sum(female_40_to_44_years),
              female_45_to_49_years = sum(female_45_to_49_years),
              female_50_to_54_years = sum(female_50_to_54_years),
              female_55_to_59_years = sum(female_55_to_59_years),
              female_60_and_61_years = sum(female_60_and_61_years),
              female_62_to_64_years = sum(female_62_to_64_years),
              female_65_and_66_years = sum(female_65_and_66_years),
              female_67_to_69_years = sum(female_67_to_69_years),
              female_70_to_74_years = sum(female_70_to_74_years),
              female_75_to_79_years = sum(female_75_to_79_years),
              female_80_to_84_years = sum(female_80_to_84_years),
              female_85_years_and_over = sum(female_85_years_and_over)) |>
    pivot_longer(cols = male_under_5_years:female_85_years_and_over, values_to = "v1", names_to = "sexage")
  
  ## ---- Females by age ----
  ## row = st_ipf_sex (sex pop for each LHD)
  ## seed = st_lhd_sexage_bind (male and female by age estimates for each LHDs)
  ## col = st_state_sexage
  seed <- st_lhd_sexage_bind |>
    select(starts_with("female"))
  rowc <- as.matrix(ipf_sex$female)
  
  st_state_sexage_female <- st_state_sexage |>
    filter(str_starts(sexage, "female")) 
  
  colc <- as.matrix(st_state_sexage_female$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  if(sum(rowc) == sum(colc)){
    st_ipf_sex_female <- ipf2(rowc, colc, seed)$fitted.table
    st_ipf_sex_female <- bind_cols(select(st_lhd_sexage_bind, naccho_id), st_ipf_sex_female) |> 
      mutate(ipf_female = 1,
             STUSAB = st)
  }else{
    st_ipf_sex_female <- bind_cols(select(st_lhd_sexage_bind, naccho_id), seed) |> 
      mutate(ipf_female = 0,
             STUSAB = st)
  }
  
  ## ---- Males by age ----
  ## row = st_ipf_sex (sex pop for each LHD)
  ## seed = st_lhd_sexage_bind (male and female by age estimates for each LHDs)
  ## col = st_state_sexage
  seed <- st_lhd_sexage_bind |>
    select(starts_with("male"))
  rowc <- as.matrix(ipf_sex$male)
  
  st_state_sexage_male <- st_state_sexage |>
    filter(str_starts(sexage, "male")) 
  
  colc <- as.matrix(st_state_sexage_male$v1)
  
  ## Run the IPF2 function (since it's a two dimensional table)
  ## Resulting data frame doesn't have any naccho_ids on them, so need to attach them
  if(sum(rowc) == sum(colc)){
    st_ipf_sex_male <- ipf2(rowc, colc, seed)$fitted.table
    st_ipf_sex_male <- bind_cols(select(st_lhd_sexage_bind, naccho_id), st_ipf_sex_male) |> 
      mutate(ipf_male = 1,
             STUSAB = st)
  }else{
    st_ipf_sex_male <- bind_cols(select(st_lhd_sexage_bind, naccho_id), seed) |> 
      mutate(ipf_male = 0,
             STUSAB = st)
  }
  
  ## Joins the st_ipf_sex_female and st_ipf_sex_female data frames together 
  st_ipf_sexage <- st_ipf_sex_male |> 
    left_join(st_ipf_sex_female)
  
  # Remove extraneous df
  if(exists("pl_lhds")){
    rm(pl_lhds)
  }
  
  if(exists("county_lhds")){
    rm(county_lhds)
  }
  
  if(exists("cousub_lhds")){
    rm(cousub_lhds)
  }
  
  ## Return st_ipf_sexage
  return(st_ipf_sexage)
}  

## ---- Iterate over all states to generate ACS sex counts ----
acs_sexage_by_state <- map(state_abbr, ~generate_acs_sexage_ipf(.x))

## ---- Collapse acs_sexage_by_state into a single data frame ----
acs_sexage <- bind_rows(acs_sexage_by_state)

## ---- Drop NA records from acs_sexage before writing out to CSV ----
acs_sexage <- acs_sexage |> 
  filter(!is.na(naccho_id))

## ---- Create nationwide output file for sex by age ----
write_csv(acs_sexage, glue("{acs_output_dir}/{lhd_vintage}_sexage.csv"))
