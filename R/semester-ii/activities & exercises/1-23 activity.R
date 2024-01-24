library(tidyverse)

rm(list = ls())



# get median income by borough --------------------------------------------

censusrx::multiyr.acs.wrapper

med.hh.incs <-
  tidycensus::get_acs(
    geography = 'county'
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
ny.inc <- med.hh.incs %>%
  filter(grepl(boros.regx, name))

ny.inc
