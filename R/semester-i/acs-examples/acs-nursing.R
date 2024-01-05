library(tidyverse)
library(sf)

visaux::nyboros
nynurses <- tidycensus::get_acs(
  geography = 'tract'
  ,state = 'NY'
  ,county = visaux::nyboros
  ,variables = c('nurses.median.earning' = 'B24011_019')
  ,geometry = T
)

library(mapview)
nynurses %>%
  mapview::mapview(zcol = 'estimate')
