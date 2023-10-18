library(tidyverse)

rm(list = ls())

# load data ---------------------------------------------------------------

#' downloaded from https://github.com/new-york-civil-liberties-union/NYPD-Misconduct-Complaint-Database-Updated
#'
#' on 9/19/2023

# path to data
pth <-
  '~/R/local-data/nypd-complaints/CCRB Complaint Database Raw 04.28.2023/CCRB Complaint Database Raw 04.28.2023.csv'
#?read.csv
pdcomplaints <- vroom::vroom(pth)
# (vroom::vroom is a faster version of read.csv)

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
colnames(pdcomplaints) # also equivalent
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

# select:   --- gets subset of COLUMNS
pdcomplaints %>%
  select(matches('Date'))

# filter:   --- gets subset of ROWS
pdcomplaints %>%
  filter(ImpactedGender == 'Female')

# mutate    --- modifies a column or set of columns
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

pdcomplaints %>% count(IncidentDate)

#' here's one way that's in the middle -- less typing, but using the same
#' conceptual approach as the `if_else` method.
#'
#' type `?case_when` to pull up documentation
#'
#' (double == signs are for True/False comparisons; single = signs are more for
#' assigning variables or arguments of functions.)
#'
#'
#' clean the gender column
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

# below two commands are the same
pdcomplaints %>%
  count(ImpactedGender)

pdcomplaints %>%
  group_by(ImpactedGender) %>%
  summarise( number.of.rows = n() )

pdcomplaints %>%
  summarise( number.of.rows = n() )



# quick vectors and indexing tutorial ------------------------------------------

## what is a vector? -------------------------------------------------------

#' a vector is basically a series of values.
#'
#' let's define a vector below -- it uses just the `c` function:
sample.vector <- c(1,2,3)
sample.vector


#' each column of a dataframe can be thought of as it's own vector: a dataframe
#' can be thought of as a collection of named vectors of equal length.

# this is also a vector:
pdcomplaints$AllegationID

another.sample.vector <- c('a','b', 'c')
another.sample.vector

# this is a way to manually create a dataframe from vectors -- as opposed to
# just reading in a csv.
sample.dataframe <- data.frame(
   numbers = sample.vector
  ,letters = another.sample.vector
)
sample.dataframe
# we can call all the same operations as on `nypdcomplaints`
sample.dataframe %>% select(numbers)

# checking data type with `class`
sample.dataframe$numbers %>% class()
sample.dataframe$letters %>% class()

# doing math with vectors -- looking at how results change
1+4
1+sample.dataframe$numbers
2*sample.dataframe$numbers

sample.dataframe$numbers + sample.dataframe$numbers
sample.dataframe$numbers + c(1,2, NA)

# finally, to sum every element of a vector
sum(sample.dataframe$numbers)

## selecting columns or checking what columns exist in a table ----------------------------

#' given table `pdcomplaints`:
#'
#' `colnames` function will list all column names
#'
#' Or we can count on autocomplete after typing a `$` after a table or calling
#' `select`
pdcomplaints %>% colnames()

pdcomplaints$OfficerGender
pdcomplaints %>% select(OfficerGender)

pdcomplaints

# indexing practice -------------------------------------------------------

#' indexing is basically selecting a subset of rows/columns
#'
#' brackets: [] allow us to index using base R; we can ask for rows and then
#' columns
#'

sample.dataframe

#' say we want to only select the second row
sample.dataframe[2, ]
# what about the second AND third rows? ----we can index with a vector
sample.dataframe[c(2, 3), ]
#' we can also use a logical vector -- Trues/Falses
#'
#' To select the second row using a logical vector:
sample.dataframe[c(F, T, F), ]

# indexing with logical vectors is relevant, because that's how we filter our
# data -- let's think about officer gender-- how do we get to only complaints
# that involved a male officer?
pdcomplaints %>% count(OfficerGender)

# we can turn the column --- which was a vector --- into a logical vector as
# below:
pdcomplaints$OfficerGender == 'Male'
pdcomplaints[ pdcomplaints$OfficerGender == 'Male' , ] # a subset of the full data

# The above is indexing in base R -- we can also use tidyverse:
pdcomplaints %>%
  filter( OfficerGender == 'Male' &
            FirstName == 'Robert'
            ) %>%
  count(TaxID) %>%
  arrange(desc(n))

complaints.per.officer <- pdcomplaints %>%
  count(TaxID) %>%
  arrange(desc(n))


# top complaint-inducing officers:
most.complained.about <-
  complaints.per.officer[complaints.per.officer$n > 80, ]$TaxID

pdcomplaints %>%
  filter(TaxID %in%
           most.complained.about
         )
#' the comma in the brakets is important, the logic is:
#'
#' data.frame[row.selection, column.selection]
pdcomplaints$ImpactedGender[1:10]



# distribution of complaints by officer -----------------------------------


#' let's think through distributions a bit more, try and make a first
#' visualization, and i think this could be a fruitful place for analysis.
#'

# (down the line we might think about filtering our universe)
pdcomplaints %>% select( where(is.Date ))
pdcomplaints$DaysOnForce %>% summary()


complaints.per.officer <-
  pdcomplaints %>%
  count(TaxID) %>%
  arrange(desc(n))

complaints.per.officer


# beginning to think through time-series analysis -------------------------------

library(lubridate)
sample.date <- ymd('2023-09-1')
sample.date %>% class()
lubridate::month(sample.date)
lubridate::year(sample.date)
# a very relevant column for a lot of data.
pdcomplaints %>% count(IncidentDate)
pdcomplaints$
pdcomplaints %>%
  mutate(
    incidentyr =
  )

pdcomplaints$IncidentDate %>% class()
pdcomplaints$OfficerGender %>% class()

lubridate

filter(pdcomplaints,
       OfficerGender == 'Male') %>%
  count(TaxID)



# .ppt code ---------------------------------------------------------------

# tidyverse select
pdcomplaints %>%
  select(CCRBDisposition, ContactOutcome)

# base R indexing
pdcomplaints[ , c('CCRBDisposition', 'ContactOutcome')]

# filter
pdcomplaints %>%
  filter(OfficerGender == 'Male')

# index
pdcomplaints[ pdcomplaints$OfficerGender == 'Male', ]


# pulling columns: base R and tidyverse
pdcomplaints$OfficerGender
pdcomplaints %>% pull(OfficerGender)


pdcomplaints %>%
  count(ContactReason, ContactOutcome)

pdcomplaints$IncidentDate %>% range(na.rm = T)
#pdcomplaints %>%

pdcomplaints %>%
  mutate(indcidentyr =
           year(IncidentDate)
         ,mindcidentmonth =
           month(IncidentDate)
         ) %>%
  filter(indcidentyr >= 2000 &
           indcidentyr < 2022) %>%
  group_by(indcidentyr, BoardCat) %>%
  summarise(n = n()) %>%
  ggplot(
    aes(x = indcidentyr
        , y = n
        ,fill = BoardCat
        ,color =BoardCat)
  ) +
  geom_area() +
  # geom_path(
  #   linewidth = 1
  # ) +
  scale_fill_manual(
    values = c(visaux::jewel.pal(), 'grey35', 'black')
    ,aesthetics = c('color', 'fill')
  ) +
  theme_bw()


pdcomplaints <- pdcomplaints %>%
  mutate(Incidentyr =
           lubridate::year(IncidentDate))

tmp <-
  pdcomplaints %>%
  group_by(Incidentyr) %>%
  summarise( n.complaints = n()
            ,n.substantiated = sum(BoardCat == 'Substantiated')
            ) %>%
  ungroup() %>%
  mutate(perc.substantiated =
           n.substantiated / n.complaints)

tmp$perc.substantiated %>% scales::percent()

tmp$n.complaints %>% scales::comma()
