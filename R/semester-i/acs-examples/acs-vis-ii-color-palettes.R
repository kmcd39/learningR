library(tidyverse)
library(sf)

rm(list = ls())

# don't worry too much about code here ---------------------------------------------

# (i'm using other code I've written to quickly pull some data for NYC) ; this
# code is the same as what we used to pull in data in the sample script last
# week.

# (the command below wraps several `tidycensus` calls)

# pull nyc data by neighborhood ("census tract")
attrs <- censusrx::get.tract.attrs(
  state = 36
  ,cofps = visaux::nyboros
  ,year = 2021
  ,geo = 'tract'
)

# get counties
cofps <-
  tigris::counties(
    state = 36
    ,year = 2021
  ) %>%
  rename_with(tolower)

# trim to just the 5-boros of NYC
boros <- cofps %>%
  filter(countyfp %in% visaux::nyboros)

# extract countyfp & add boro name to the tract-level data.
attrs <- attrs %>%
  mutate(cofp = substr(geoid, 3,5)
         ,.after = geoid)

attrs <- attrs %>%
  inner_join(tibble(boros)[c('countyfp', 'name')]
             ,by = c('cofp' = 'countyfp'))

attrs <- attrs %>%
  rename(boro = name)

## also get cbsa median income ----------------------------------------------

boros
# the CBSA is a metropolitan area
cbsa.meds <-
  censusrx::pull.tidycensus.median.tables(
    #states = '36'
    years = 2021
    ,geo = 'cbsa'
  )

#' 35620 is the geoid for New York metro area (census bureau) --
#'
#' https://www2.census.gov/programs-surveys/cps/methodology/2015%20Geography%20Cover.pdf
cbsa.med.hh.inc <-  cbsa.meds$med.hhinc %>%
  filter(geoid == '35620')

# # (to look at the outlines of the metro area) --
# library(mapview)
# tigris::core_based_statistical_areas(year = 2021) %>%
#   filter(GEOID ==
#            '35620') %>%
#   mapview()



# result of the above -----------------------------------------------------

#' the data represents Neigborhoods ("census tracts") in New York City.
#'
#' There's one row per neighborhood in the data; columns represent information
#' for those neighborhoods.
attrs

# separately, a 1-row dataframe with the NY metro area median income -- we'll be
# able to use this to provide context.
cbsa.med.hh.inc

# using diverging color palettes ------------------------------------------

attrsf <- attrs %>%
  geox::attach.geos(year = 2021)


# basic plot:
attrsf %>%
  filter(as.numeric(aland.acre) >
           0) %>%
  ggplot(
    aes(fill = med.hhinc)
  ) +
  geom_sf(color = NA)

# using viridis
attrsf %>%
  filter(as.numeric(aland.acre) >
           0) %>%
  ggplot(
    aes(fill = med.hhinc)
  ) +
  geom_sf(color = NA) +
  scale_fill_viridis_c()
# better :)


# using a diverging scale
library(colorspace)
hcl_palettes(type = "diverging")
attrsf %>%
  filter(as.numeric(aland.acre) >
           0) %>%
  ggplot(
    aes(fill = med.hhinc)
  ) +
  geom_sf(data = attrsf
          ,color = 'grey35'
          ,fill = 'grey35') +
  geom_sf(color = NA) +
  scale_fill_continuous_diverging(
    palette = 'Purple-Green'
    ,mid = cbsa.med.hh.inc$n
  ) +
  theme_dark()

cbsa.med.hh.inc

# using categorical palettes ----------------------------------------------

# default
attrs %>%
  ggplot() +
  geom_histogram(
    aes(fill = boro
        ,x = med.hhinc)
    ,binwidth = 10e3
  )


# Rcolor brewer
#library(RColorBrewer)
attrs %>%
  ggplot() +
  geom_histogram(
    aes(fill = boro
        ,x = med.hhinc)
    ,binwidth = 10e3
  ) +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(5,'Dark2')
  )

# a palette i made...
attrs %>%
  ggplot() +
  geom_histogram(
    aes(fill = boro
        ,x = med.hhinc)
    ,binwidth = 10e3
  ) +
  scale_fill_manual(
    values = visaux::jewel.pal()
  )
# (extra credit if you make some good, color blind accessible discrete color
# palettes :)




# surveying colors --------------------------------------------------------


RColorBrewer::brewer.pal(5,'Dark2') %>%
  scales::show_col()

visaux::jewel.pal() %>%
  scales::show_col()


