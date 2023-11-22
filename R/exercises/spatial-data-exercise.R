library(tidyverse)
# install.packages('sf')

# the package `sf` will underlie nearly all spatial work in R!
library(sf)


# the package `tigris` allows us to quickly pull geometries from the census
# bureau
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

#

# st_crs asks what coordinate reference system, or map projection, the data is
# using
cos %>% st_crs()

# st_transform puts the data into a new CRS.
cos %>%
  st_transform(4326) %>%
  st_crs()



# to load a shapefile -----------------------------------------------------

ddir <- 'local-data/Borough Boundaries/'
fn <- list.files(ddir, pattern = 'shp')

boros <- paste0(ddir, fn) %>%
  st_read()


library(mapview)
boros %>% mapview(zcol = 'boro_name')




# example tidycensus pull -------------------------------------------------


#' tidycensus vignette:
#' https://walker-data.com/tidycensus/articles/basic-usage.html
#'
#' (we will need the API key)

# install.packages('tidycensus')
install.packages('tigris')

acs.vars <-
  tidycensus::load_variables(
     2021
    ,dataset = 'acs5'
  )


acs.vars

med.hh.inc <-
  tidycensus::get_acs(
     geography = 'tract'
    ,variables = c('med.hh.inc' ='B19013_001')
    ,year =2021
    ,state ='NY'
    ,survey = 'acs5'
    ,geometry = T
  )

med.hh.inc %>%
  ggplot(
    aes(fill = estimate)
  ) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(
    name = 'Median household income'
  )



med.hh.inc %>% mapview(zcol = 'estimate')
