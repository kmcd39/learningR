library(tidyverse)

# the package `sf` will underlie nearly all spatial work in R!
# install.packages('sf')
library(sf)


# the package `tigris` allows us to quickly pull geometries from the census
# bureau
#install.packages('tigris')
cos <- tigris::counties(state = 'NY', year = 2021)

# we can notice that the spatial data is the data.frame class AND the `sf` class
cos %>% class()

# we can manipulate the spatial data in the same way as other tables, for the
# most part:
colnames(cos)
cos %>% glimpse()
cos %>%
  filter(NAME == 'Kings')

# and plot it with ggplot! (just use geom_sf)
cos %>%
  ggplot(aes(fill = NAME)) +
  geom_sf()


# coordinate reference systems (CRS) --------------------------------------------

# st_crs asks what coordinate reference system, or map projection, the data is
# using
cos %>% st_crs()

# st_transform puts the data into a new CRS.
cos %>%
  st_transform(4326) %>%
  st_crs()

# to load a shapefile -----------------------------------------------------

#' this was downloaded from NYC OpenData as a shapefile:
#'
#' https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm
ddir <- 'local-data/Borough Boundaries/'
fn <- list.files(ddir, pattern = 'shp')

boros <- paste0(ddir, fn) %>%
  st_read()

# mapview is very useful for quick, interactive maps: the `zcol` argument should
# be a column in the data that will correspond to the fill color for the map.
library(mapview)
boros %>% mapview(zcol = 'boro_name')


# example tidycensus pull -------------------------------------------------


#' tidycensus vignette:
#' https://walker-data.com/tidycensus/articles/basic-usage.html

# install.packages('tidycensus')
acs.vars <-
  tidycensus::load_variables(
     2021
    ,dataset = 'acs5'
  )

# (it starts out looking slightly messy, with exclamation points instead of
# spaces, but we can clean up pretty quickly)
acs.vars <- acs.vars %>%
  mutate(label = # replaces the double-! with space
           gsub('!!', ' ', label)) %>%
  mutate(label = # remove trailing colons (`$` means the "end of the string" in regex)
           gsub(':$', '', label)
         )

# I have this saved on my computer so i can quickly look at the available ACS
# tables:
acs.vars
#acs.vars %>% write.csv(file = '~/acs-variables.csv')

# to query median household income -- we can identify that the variable
# "B19013_001" corresponds with median income from the variable list we just
# pulled, or we could google it or look on websites like censusreporter.com
# (https://censusreporter.org/tables/B19013/)
med.hh.inc <-
  tidycensus::get_acs(
     geography = 'tract'
    ,variables = c('med.hh.inc' ='B19013_001')
    ,year =2021
    ,state ='NY'
    ,survey = 'acs5'
    ,geometry = T
  )

# to make some quick maps!

# static map:
med.hh.inc %>%
  ggplot(
    aes(fill = estimate)
  ) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(
    name = 'Median household income'
  )

# interactive map:
med.hh.inc %>% mapview(zcol = 'estimate')
