library(tidyverse)
library(sf)
options(tigris_use_cache=TRUE)

rm(list = ls())


# to make sure we have the packages installed: ---------------------------------

# (we only have to install once, until we want to update or are on a different
# computer)

# install.packages('tidycensus')
# install.packages('tigris')
# install.packages('sf')

# get median income by borough --------------------------------------------

# get NY counties (areas and name)
ny.cos <- tigris::counties(state =36, year = 2021) %>%
  rename_with(tolower)

# get NY neighborhood-level income
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



# activity! ---------------------------------------------------------------

# let's see how many nbhds are in each boro -- mostly to check that we have the
# boros we expect.
ny.inc %>%
  count(name)

#' next let's make a histogram of neighborhood incomes in NYC
#'

ny.inc %>%
  ggplot(
    aes(
      x = estimate
      )
  ) +
  geom_histogram(
    fill = '#008888'
    ,binwidth = 5000
  )


## what about the breakdown by borough? ------------------------------------

ny.inc %>%
  ggplot(
    aes(
      x = estimate
    )
  ) +
  geom_histogram(
    fill = '#008888'
    ,binwidth = 5000
  ) +
  facet_wrap(
    vars(name)
    ,ncol = 1
  )


ny.inc %>%
  ggplot(
    aes(
      x = estimate
      ,fill = name
    )
  ) +
  geom_histogram(
    binwidth = 5000
  )




