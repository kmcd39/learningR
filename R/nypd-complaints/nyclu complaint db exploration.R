library(tidyverse)

rm(list = ls())

# load data ---------------------------------------------------------------

#' downloaded from https://github.com/new-york-civil-liberties-union/NYPD-Misconduct-Complaint-Database-Updated
#'
#' on 9/19/2023

# path to data
pth <-
  '~/R/local-data/nypd-complaints/CCRB Complaint Database Raw 04.28.2023/CCRB Complaint Database Raw 04.28.2023.csv'

pth

#?read.csv
pdcomplaints <- read.csv(pth)
pdcomplaints <- tibble(pdcomplaints)

pdcomplaints

# some basics -------------------------------------------------------------

#' the %>% pipe operator passes a variable on to be first argument in the
#' function
#'
#' Below, `glimpse` is the function, and it takes `pdcomplaints` as the
#' argument, and that is expressed two different ways that will have the same
#' result.
glimpse(pdcomplaints)
pdcomplaints %>% glimpse()

pdcomplaints %>% colnames()

pdcomplaints %>% nrow()

# the dollar sign `$` "indexes a table and selects a given column.
pdcomplaints$OfficerGender


# goals! ------------------------------------------------------------------

#' what are our goals for this data analysis? What interesting questions might
#' we be able to answer or start to shed light on? What data points might we or
#' our audience be interested in?
#'

#' brainstorming:
#'
#' -- gender of person who lodged complaint and where it took place.
#'
#' -- have complaints increased when the size of the police force has increased?
#'
#' -- how concentrated or dispersed are complaints across different officers? How
#' have complaints changed over time

#' --what proportion of complaints end in discipline vs getting dismissed? any patterns among those?
#'
#' --What have been types of allegations? how does that change based on area? Hypothesis that this might be shaped by underlying relationships w/ police that might vary by nbhd
#'
#' --patterns of complaints based on officers' race?
#'
#' --(other things we might consider interesting as we go!)


# gender of person who lodged complaint -----------------------------------

# this might be relevant column...
pdcomplaints$ImpactedGender

#' `count` is a very useful command in the tidyverse that counts the number of
#' times each value shows up in a given column.
pdcomplaints %>% count(ImpactedGender)

#' dang. We see that the data is kind of messy. There are a lot of blanks. And
#' there are separate categories that should actually overlap.

#' first, let's think about the blanks. Oh -- the data dictionary tells us it is
#' empty before the year 2000.
pdcomplaints$CloseDate
pdcomplaints$IncidentDate

#' we can see there are multiple "date" columns, and the data dictionary sheds
#' light on what each one is.
pdcomplaints %>%
  select(CloseDate
         ,IncidentDate
         ,AsOfDate)

#' or, the below selects all columns where the string "Date" appears.
#'
#' (a string is a sequence of characters --- letters or numbers)
pdcomplaints %>%
  select(matches('Date'))


#' let's clean the messiness in this ImpactedGender column
pdcomplaints %>% count(ImpactedGender)

# so we want to combine the categories that should clearly overlap.





# essential verbs for tidy data manipulation ------------------------------

# select:
pdcomplaints %>%
  select(matches('Date'))

# filter
pdcomplaints %>%
  filter(ImpactedGender == 'Female')

# mutate
pdcomplaints %>% count(ImpactedGender)
pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           str_extract(ImpactedGender #( this line we don't have to worry about so much yet)
                       ,'Male|Female|TGNC.*Other')
           )


# we reassigned or mutated the ImpactedGender column, so now our `count` command
# gets a different result:
pdcomplaints %>% count(ImpactedGender)


#' (double == signs are for True/False comparisons; single = signs are more for
#' assigning variables or arguments of functions.)

pdcomplaints



# data exploration --------------------------------------------------------

pdcomplaints <- pdcomplaints %>% tibble()

pdcomplaints %>% colnames()
pdcomplaints %>% count(PenaltyCat)

pdcomplaints %>% count(OfficerRace)

# initial analysis --------------------------------------------------------


# visualizations ----------------------------------------------------------


