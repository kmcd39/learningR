#' TODO:
#'
#' Fill out this script to deal with things like: very low population tracts,
#' stressing population density, illustrating empty space and MAUP problems.
#'
#' Also, capping at percentile and such so that color scale doesnt get washed out..!



# population densities ----------------------------------------------------



bgpops <-
  map2_dfr(ny.metro.cos$statefp
           ,ny.metro.cos$countyfp
           , ~tidycensus::get_acs(
             geography = 'block group'
             ,variables = c('pop' = 'B02001_001')
             ,year = 2021
             ,state = .x
             ,county = .y
             ,survey = "acs5")
  ) %>%
  rename_with(tolower)

bgpops <- bgpops %>%
  select(-name)

# we can join the geometries in that we got from tigris:
nybgs
bgpopsf <- bgpops %>%
  left_join(nybgs[c(#'statefp', 'countyfp',
    'geoid', 'aland', 'awater')]
    ,by = c('geoid') )


bgpopsf$aland %>%
  head(5) %>%
  units::set_units('m^2') %>%
  units::set_units('acres')


bgpopsf <- bgpopsf %>%
  mutate(pop.dens =
           as.numeric(
             estimate /
               units::set_units(
                 units::set_units(aland, 'm^2')
                 ,'acres'))
         ,.after = estimate
  )

# there's a few issues here:
