library(tidyverse)
library(sf)

rm(list = ls())

# get median income by borough --------------------------------------------

ny.cos <- tigris::counties(state =36, year = 2021) %>%
  rename_with(tolower)


med.hh.incs <-
  tidycensus::get_acs(
    geography = 'tract'
    #,table = 'B02001'
    ,variables = c('med.hh.inc' = 'B19013_001')
    ,year = 2021
    ,state = 36
    ,survey = "acs5") %>%
  rename_with(tolower)

med.hh.incs

# let's just use the 5 boros of NYC for simplicity -- we can use Regex to
# filter!
boros.regx <- 'Kings|Queens|^New York|Richmond|Bronx'
#visaux::nyboros

ny.inc <-
  med.hh.incs %>%
    select(-name) %>%
    mutate(county.fips = substr(geoid,3,5)
           ) %>%
  filter(substr(geoid,3,5) %in%
           c("085", "005", "047", "061", "081") # filter to 5 boros by FIPS code
           ) %>%
    left_join(tibble(ny.cos)[c('countyfp', 'name')]
              ,by = c('county.fips' = 'countyfp'))

ny.inc

