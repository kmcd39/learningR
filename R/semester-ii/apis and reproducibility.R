library(tidyverse)


# APIs --------------------------------------------------------------------

#' an API is an "application programming interface" -- which is a very vague
#' acronym. In data analysis contexts, it's basically a way to pull data that is
#' sorted/filtered/prepared a specific way directly with code.
#'
#' I.e., instead of going to https://data.census.gov/, finding your table,
#' downloading and saving on your computer, and porting into R or excel or
#' whatever, you write a line of code that references & pulls that table.
#'
#' The major advantage of working with APIs are ~convenience~ and
#' ~reproducibility~.
#'
#' It's more convenient to adjust small aspects of what you pulled (everything
#' the same, but actually an extra few years) -- change one thing in your script
#' instead of doing the whole export again.
#'
#' Reproducibility is valued in academic and scientific research -- because it's
#' hard to verify experiments that are not repeatable. But in general
#' reproducibility is a good practice in making research transparent. It's also
#' helpful for coming back to and adapting old work, because you won't have to
#' organize the data and make sure you pull it the exact same way when you want
#' to update the analysis or if you're working on a different computer!
#'
#' There are many different APIs! And using them may be different! In general,
#' there is a separate R package designed to make using the API relatively
#' painless. Some APIs may not have an R package (will be rare!) -- they may
#' still be usable, but it may be harder.
#'
#' Often (as we've seen), using an API will require getting some login
#' credentials set up. In general, once they'r setup once, we can leave them be
#' forever.


# tidycensus --------------------------------------------------------------


#' the `tidycensus` package we've been using just links to the Census API!
#'
tidycensus::get_acs(
  geography = 'county'
  ,table = 'B02001'
  ,year = 2022
  ,state = 36
  ,survey = "acs5") %>%
  rename_with(tolower)



# RSocrata ----------------------------------------------------------------

#' links to many cities' or other government open data portals...
#'
#' WE can find the API endpoint on open data and pull directly w RSocrata.
#'
#' For school demographic data (at
#' https://data.cityofnewyork.us/Education/2019-20-Demographic-Snapshot-School/nie4-bv6q/about_data),
#' the endpoint was: https://data.cityofnewyork.us/resource/nie4-bv6q.json
sch <-
  RSocrata::read.socrata('https://data.cityofnewyork.us/resource/nie4-bv6q.json')

sch %>% tibble()



# closing -----------------------------------------------------------------


#' there are countless other APIs! For getting data from different sources but
#' also for other things like doing some more complex calculations  (like
#' geocoding addresses, or figuring out routing from place to place). Some of
#' these APIs may cost money (we won't use any in this class.)
#'
#'
