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
pdcomplaints <- vroom::vroom(pth) # vroom::vroom is a faster version of read.csv

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
#?colnames
#?nrow

pdcomplaints %>% nrow()

# the dollar sign `$` "indexes a table and selects a given column.
pdcomplaints$OfficerGender

# Research questions -------------------------------------------------

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

#' here, we're cleaning the data by redefining the duplicated terms Female and
#' Female/Woman to be single categories.

# below uses a very fast, streamlined approach, but it depends on "regular
# expressions" (regex for short). These may prove useful, but don't worry about
# them for now.
pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           str_extract(ImpactedGender #( this line we don't have to worry about so much yet)
                       ,'^Male$|^Female$|TGNC.*Other'
                       )
         )


# here's another conceptually simpler, but more verbose way of doing the same
# thing

# (start with a fresh load of the data)
rm(list = ls())
path.to.data <-
  '~/R/local-data/nypd-complaints/CCRB Complaint Database Raw 04.28.2023/CCRB Complaint Database Raw 04.28.2023.csv'
pdcomplaints <- vroom::vroom(path.to.data)

# # thinking through the difference in the following two lines of code:
# pdcomplaints %>% count(ImpactedGender)
# pdcomplaints <- pdcomplaints %>% count(ImpactedGender)

#' when we assigned the output from `count` to `pdcomplaints`, we overwrote it.
#' If we didn't want that, we have to reload the data -- by re-running our
#' earlier line of code that had loaded it originally.
#'
#' Additionally, we we assign a new value (` <- `) in a line of code, there's no
#' output -- or the output is given to the assigned value. When we don't assign,
#' the output is displayed but not retained.

pdcomplaints

pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           if_else(ImpactedGender == 'Female/Woman' # we are looking for places where the column is equal to "Female/Woman"
                   , 'Female' # We replace the value where that occurs with "Female" --- more simply, we are replacing "Female/Woman" with just "Female"
                   ,  ImpactedGender # where we don't see that, we leave things the same
                   )
         ) %>%
  mutate(ImpactedGender =
           if_else(ImpactedGender == 'Male/Man'
                   ,'Male'
                   , ImpactedGender  )
  )

pdcomplaints %>% count(ImpactedGender)

#' here's one way that's in the middle -- less typing, but using the same
#' conceptual approach as the `if_else` method.
#'
#' type `?case_when` to pull up documentation
#'
#' (double == signs are for True/False comparisons; single = signs are more for
#' assigning variables or arguments of functions.)
pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           case_when(
              ImpactedGender == 'Female/Woman' ~ 'Female'
             ,ImpactedGender == 'Male/Man' ~ 'Male'
             ,TRUE ~ ImpactedGender )
           )

# we reassigned or mutated the ImpactedGender column, so now our `count` command
# gets a different result:
pdcomplaints %>% count(ImpactedGender)

pdcomplaints %>% select(matches('date'))
pdcomplaints %>% count(IncidentPrecinct)



#' next essential functions: group_by and summarise

# group_by
pdcomplaints %>%
  group_by(ImpactedGender) %>%
  summarise( number.of.rows = n() )

pdcomplaints %>%
  summarise( number.of.rows = n() )

pdcomplaints

# quick vectors and indexing tutorial ------------------------------------------

pdcomplaints$ImpactedGender[1:10]

# beginning to think through time-series analysis -------------------------------

library(lubridate)

# a very relevant column for a lot of data.
pdcomplaints %>% range()
pdcomplaints$IncidentDate

pdcomplaints$IncidentDate %>% class()


lubridate

