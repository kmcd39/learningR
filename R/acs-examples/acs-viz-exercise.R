library(tidyverse)
library(sf)


rm(list = ls())


# don't worry about code here ---------------------------------------------

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
boros <- cofps %>%
  filter(countyfp %in% visaux::nyboros)

# extract countyfp & add boro name
attrs <- attrs %>%
  mutate(cofp = substr(geoid, 3,5)
         ,.after = geoid)

attrs <- attrs %>%
  inner_join(tibble(boros)[c('countyfp', 'name')]
             ,by = c('cofp' = 'countyfp'))



# histograms/density plots of NYC nbhds --------------------------------------------------

#attrs <- attrs %>% filter(pop > 50)

attrs %>%
  ggplot(
    aes(x = med.hhinc)
  ) +
  geom_histogram(
    binwidth = 5e3
    ,fill = '#880000'
  )

attrs

attrs %>%
  ggplot(
    aes(x = med.hhinc
        ,fill = name
        ,weight = n.hh)
  ) +
  geom_histogram(
    binwidth = 5e3
  ) +
  scale_fill_manual(
    values = visaux::jewel.pal()
  )


## Making use of another package! ggridges ---------------------------------

# vignette:
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#install.packages('ggridges')
library(ggridges)

#ggridges::geom_density_line()
attrs %>%
  ggplot(
    aes(x = med.hhinc
        ,fill = name
        #,color = name
        ,y = name
        ,group = name
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


attrs %>%
  ggplot(
    aes( x = med.hhinc
        ,y = perc.no.car
        ,color = name
        )
  ) +
  # geom_smooth(
  #   method = 'lm',
  #   se = T ) +
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
    range = c(.3,.9)
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


# basic example
tmp %>%
  filter(year %in% 2011) %>%
  ggplot(
    aes(y = boro
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
        ,x = n
        ,fill = boro)
  ) +
  geom_col() +
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



# # adding in HHs -----------------------------------------------
#
# # get total HHs as well
# totts <- censusrx::gett.census.totals(
#   state = 36
#   ,geo = 'county'
#   ,years = c(2021)
#   ,cofps = boros$countyfp
# )
#
# # merge it in and rename medhh income column
# tmphh <- tmp %>%
#   filter(year == 2021) %>%
#   left_join(totts)  %>%
#   rename(med.hhinc = n)
#
