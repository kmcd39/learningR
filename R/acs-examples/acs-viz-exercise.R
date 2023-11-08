library(tidyverse)
library(sf)

rm(list = ls())

# don't worry too much about code here ---------------------------------------------

# (i'm using other code I've written to quickly pull some data for NYC)

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


# result of the above -----------------------------------------------------

#' the data represents Neigborhoods ("census tracts") in New York City.
#'
#' There's one row per neighborhood in the data; columns represent information
#' for those neighborhoods.
attrs

# histograms/density plots of NYC nbhds --------------------------------------------------

attrs <- attrs %>%
  rename(boro = name)
#attrs <- attrs %>% filter(pop > 50)
attrs %>% count(name)

# very basic histogram of nbhd median hh incomes
attrs %>%
  ggplot(
    aes(x = med.hhinc
        )
  ) +
  geom_histogram()

# more happening here - but it's still just a histogram showing the same
# variable.
second.histogram <-
  attrs %>%
  ggplot(
    aes( x = med.hhinc
        ,fill = boro
        ,weight = n.hh # n.hh = "number of households"
        )
  ) +
  geom_histogram(
    binwidth = 5e3 # 5e3 is scientific notation for 5,000
  ) +
  scale_fill_manual(
    values = visaux::jewel.pal()
  )

second.histogram

second.histogram +
  facet_wrap(vars(boro)
             ,ncol = 1)

## Making use of another package! ggridges ---------------------------------

# vignette:
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#install.packages('ggridges')
library(ggridges)


attrs %>%
  ggplot(
    aes(x = med.hhinc
        ,fill = boro
        #,color = name
        ,y = boro
        ,group = boro
        ,weight = n.hh)
  ) +
  geom_density_ridges() +
  scale_y_discrete(
    'Boro'
  ) +
  scale_x_continuous(
    name = 'Neighborhood Median\nHousehold Income'
    ,labels = scales::comma
  ) +
  scale_fill_manual(
    values = visaux::jewel.pal()
    ,aesthetics = c('fill', 'color')
  ) +
  theme_bw()


# scatter plots ------------------------------------------------------------

# we can think about how income correlates with... car ownership!
attrs
attrs %>% colnames()


attrs %>%
  ggplot(
    aes( x = med.hhinc
        ,y = perc.no.car
        ,color = boro
        )
  ) +
  geom_smooth(
    method = 'lm',
    se = F) +
  geom_point(
     aes(size = n.hh)
    ,alpha = .6
    #,show.legend = F
  ) +
  scale_fill_manual(
    'Boro'
    ,values = visaux::jewel.pal()
    ,aesthetics = c('fill', 'color')
  ) +
  scale_size_continuous(
    range = c(.9,1.8)
    ,guide = 'none'
  )

# are there any other relationships we want to explore? -------------------------





# just a column plot -------------------------------------------

boros
medians <-
  censusrx::pull.tidycensus.median.tables(
    state = 36
    ,geo = 'county'
    ,years = c(2011,2021)
    ,cofps = boros$countyfp
  )

# add boro names again
tmp <- medians$med.hhinc %>%
  mutate(cofp = substr(geoid, 3,5)
         ,.after = geoid) %>%
  inner_join(tibble(boros)[c('countyfp', 'name')]
             ,by = c('cofp' = 'countyfp')) %>%
  rename(boro = name
         ,med.hhinc = n)

# this is another dataframe -- only 10 rows; each corresponds to one boro/year
tmp

# basic example
tmp

tmp %>%
  filter(year %in% 2011) %>%
  ggplot(
    aes( y = boro
        ,x = med.hhinc
        ,fill = boro)
  ) +
  geom_col()

## example w "faceting" -----------------------------------------------------

# Plot by year instead of filtering to year
facet.colplot <- tmp %>%
  # filter(year %in% 2011) %>%
  ggplot(
    aes(y = boro
        ,x = med.hhinc
        ,fill = boro)
  ) +
  geom_col(  ) +
  facet_wrap(vars(year)
             ,ncol = 1)


facet.colplot

## and some neatening... ---------------------------------------------------

# notice we're starting with the same plot as above
facet.colplot +
  scale_x_continuous(
    'Median Household Income'
    ,labels = scales::comma
  ) +
  scale_y_discrete(
    name = NULL
  ) +
  scale_fill_manual(
    NULL
    ,values = visaux::jewel.pal()
  ) +
  theme_bw() +
  labs(
    title =
      strwrap(width = 70,
              'Household incomes increased over the past decade, with some variation across boroughs.')
    ,subtitle = 'NYC Median household income by borough, 2011 & 2021.'
    ,caption = 'Source: ACS 5-year estimates'
  )


# maps --------------------------------------------------------------------

# (we'll get more into maps later)
library(sf)
tmp
manhattan.median <- tmp %>%
  filter(year == 2021 & boro == 'New York') %>%
  .$med.hhinc

# below attaches neighborhood geometries
attrsf <- attrs %>%
  geox::attach.geos()

attrs
attrs %>% glimpse()
attrs
library(colorspace)

attrsf %>%
  filter( as.numeric(aland.acre) >
            0) %>%
  ggplot() +
  geom_sf(
    aes( fill = med.hhinc)
    ,color = NA
  ) +
  #scale_fill_viridis_c()
  scale_fill_binned_divergingx(
    mid =
      #manhattan.median
      75e3
    ,pal = 'Temps'
    ,rev = T
  )


