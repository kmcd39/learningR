library(tidyverse)

# load data ---------------------------------------------------------------

# an example from the Wickham paper
# (https://vita.had.co.nz/papers/tidy-data.pdf) as a csv
rel <-
  read.csv('local-data/untidy-table.csv') %>%
  tibble()

rel

# peek --------------------------------------------------------------------

rel


# neaten column names
rel <- rel %>%
  rename_with(
    ~trimws(
      str_replace(.x
                  ,'(\\.|X)+', ' '))
  ) %>%
  rename_with(
    ~str_replace(
      .x,
      '\\.', ' to '
    )
  )

rel

# tidy --------------------------------------------------------------------

#' regular expressions are often very useful for tidying data!
#'
#' We'll use the function `?pivot_longer`
#'
rel

rel %>% head()

# new object can be trel for tidy religion
trel <- rel %>%
  pivot_longer(
    cols = 2:ncol(.)
    ,names_to = 'income.bucket'
    ,values_to = 'freq'
  )

# using a regular expression to specify which columns:
trel <- rel %>%
  pivot_longer(
     cols = matches('k$')
    ,names_to = 'income.bucket'
    ,values_to = 'freq'
  )


trel


# a review and primer on selecting columns --------------------------------


# we can refer to columns a number of different ways...

# ...by numeric index
rel %>%
  select( 2:4 )

# ...with a regular expression or select helper:
rel %>%
  select( matches('k$'))

rel %>%
  select(ends_with('k'))

# ...with the column name itself:
rel %>%
  select("Under 10k")

rel %>%
  select(religion)

# ... or with selection rules based on what's in the column:
# (a little more rare and more complicated, but can be very useful)
rel %>%
  select( where(is.numeric) )

rel %>%
  select( where(is.character ))

# (select columns where the sum of their values is greater than 2000.)
rel %>%
  select( where(is.numeric)) %>%
  select( where( ~sum(.x) > 2000) )

# we can use all of these selection rules to refer to columns within
# pivot_longer, within rename_with, with select, etc..!

# difference in how we can analyize: -------------------------------------

# this subsection will show some differences in working with the tidy vs untidy
# data

rel
trel

## what if we want the total by religion? -------------------------

rel

# Untidy totals by religion:
rel %>%
  mutate( tot = rowSums(rel[2:ncol(rel)]) )
# it's not too bad, but we need to know which columns we're summing by and
# specify explicitly (all but the first)

# Tidy totals by religion:
trel %>%
  group_by(religion) %>%
  summarise(tot = sum(freq))
# a familiar synatax, we don't have to specify a column range.

# total by income bucket: -------------------------------------------------

rel

# Untidy totals by income:
colSums(rel[2:ncol(rel)])
# we change the function and specify a set of columns

# Tidy totals by religion:
trel %>%
  group_by(income.bucket) %>%
  summarise(tot = sum(freq))
# Nothing changes except the grouping variable.

# with tidy data, we can also add manipulations as new column onto the same
# table:
trel <- trel %>%
  group_by(religion) %>%
  mutate(total.by.religion =
           sum(freq)
         ,share.by.religion =
           freq / sum(freq)
         ) %>%
  ungroup()


# plotting: ---------------------------------------------------------------

library(hrbrthemes)

# specify factor levels:
trel  <- trel %>%
  arrange(total.by.religion) %>%
  mutate(religion =
           factor(religion,
                  levels = unique(.$religion))
         )

trel <- trel %>%
  mutate(income.bucket =
           factor(income.bucket,
                  levels = colnames(rel[2:ncol(rel)])
                  ))

trel %>%
  ggplot(
    aes(y = religion
        ,x = freq
        ,fill = income.bucket)
  ) +
  geom_col(position = position_stack(reverse = T)) +
  scale_fill_viridis_d(
    name = "Income range"
  ) +
  theme_ipsum(grid = 'X')


trel %>%
  ggplot(
    aes(y = religion
        ,x = share.by.religion
        ,fill = income.bucket
        #,color = income.bucket
        #,size = freq
        )
  ) +
  geom_col(position = position_stack(reverse = T)) +
  scale_fill_viridis_d(
    name = "Income range"
    ,aesthetics = c('color', 'fill')
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_ipsum(grid = 'X')


# more complicated example ------------------------------------------------


## pull and peeks w school demographic data -----------------------------------

# from:
# https://data.cityofnewyork.us/Education/2019-20-Demographic-Snapshot-School/nie4-bv6q

url <-
  'https://data.cityofnewyork.us/resource/nie4-bv6q.csv'
  #'https://data.cityofnewyork.us/api/views/nie4-bv6q/rows.csv?date=20240103&accessType=DOWNLOAD'
# read data for url; object is schd for school demographics

# this gets the data directly from the Open Data API, rather than first
# downloading the CSV, saving on the local computer, and reading that:
schd <-
  RSocrata::read.socrata( url )

schd <- schd %>% tibble()

# check duplicates of school names or DBNs
schd %>%
  count(dbn, school_name) %>%
  map_dbl( ~sum(duplicated(.x)) )
# (some schools will share the same names)

# (we can peek at them this way)
schd %>%
  select(dbn, school_name) %>%
  distinct() %>%
  filter(school_name %in%
           .[duplicated(.$school_name), ]$school_name
         ) %>%
  arrange(school_name)

# let's say we're interested in race/ethnicity -- we can trim other columns for
# now for simplicity
demo.regx <- 'asian|black|hispanic|multiple_race|white'

schd.trimmed <- schd %>%
  select(dbn, school_name, year, total_enrollment,
         matches( demo.regx ))

schd.trimmed

## pivoting longer ---------------------------------------------------------

# the dataset has two columns for each demographic group - i.e., "asian" and
# "asian_1." From the documentation, we can see that the ones with the "_1"
# suffix show percent. We can check a few values by dividing the number by group
# by total enrollment to make sure this is right.

#' Right now the dataset is LONG by dbn (a school identifier), school_name, and
#' year. We might want it long by demographic as well, which would help us plot
#' in ggplot.
#'
#' To get it longer, of course we want to pivot long. There will be a few ways
#' of doing it: We could drop percent first, and then recalculate it once it's
#' long:
tmp <- schd.trimmed %>%
  select(-matches('_1$')) %>%
  pivot_longer(
    matches( demo.regx )
    ,names_to = 'demographic'
  ) %>%
  group_by(dbn, year) %>%
  mutate(perc = value / total_enrollment)

tmp

#' or (this will be harder here but will show a technique that may be necessary
#' in other cases) --- we can pivot long by two categories, using a regex to
#' split the columns we're pivoting by:
#'
tmp <- schd.trimmed %>%
  pivot_longer(
    matches( demo.regx )
    # below matches two groups: the first is letters and underscore; the second
    # is either the end of the string or "_1" at the end of the string. We
    # separate the two matched identifiers to two new columns, for demographic
    # and the variable
    ,names_pattern = '([a-z_]+)($|_1$)'
    ,names_to = c('demographic', 'var')
  ) %>%
  mutate(var =
           case_when(
             # now we can rename the variable, which will show either count or
             # percent (depending on whether the "_1" was matched)
             var == '' ~ 'count'
             ,var == '_1' ~ 'perc'
           )
         )



# tidying with regex  -----------------------------------------


#' the package `untidydata` bundles "untidy datasets made for the purpose of
#' teaching the tidyverse."
#'
#' the `pre_post` dataset has a column spec, with "two grouping variables
#' separated by underscores"
?untidydata::pre_post

messy.data <- untidydata::pre_post
messy.data <- messy.data %>% tibble()

messy.data

# how do we get the g1/g2 and the hi/lo as two sets of variables?
separated.spec <- messy.data$spec %>%
  str_split('_')

separated.spec %>% head()

messy.data %>%
  mutate(gspec =
           map_chr(separated.spec, 1)
         ,spec2 =
           map_chr(separated.spec, 2)
         ,.after = spec
         )



# totals mixed in ---------------------------------------------------------

#' pull example race/ethnicity data from ACS using `tidycensus`
#'
#' about the table: https://censusreporter.org/tables/B02001/
?tidycensus::get_acs
demos <-
  tidycensus::get_acs(
     geography = 'county'
    ,table = 'B02001'
    ,year = 2022
    ,state = 36
    ,survey = "acs5") %>%
  rename_with(tolower)

# let's just use the 5 boros of NYC for simplicity -- we can use Regex to
# filter!
boros.regx <- 'Kings|Queens|^New York|Richmond|Bronx'
nydemos <- demos %>%
  filter(grepl(boros.regx, name))

# make sure we did it right!
nydemos %>%
  count(geoid, name)

# we can also clean the Boro names... with regex!
nydemos$name %>% head()
nydemos <- nydemos %>%
  mutate(name = str_extract(name, boros.regx))
# (can you think of a second way of doing this?)
nydemos

# get labls
acs.vars <- tidycensus::load_variables(year = 2022
                                       ,dataset = 'acs5')

acs.vars
# clean the labels using regex
acs.vars$label <- acs.vars$label %>%
  gsub('!!', ' ', .) %>%
  gsub('Estimate |:$|Total: ', '', .)

# for the tabel we pulled:
acs.vars %>% filter(grepl('^B02001', name))

# We can join the labels to the data now.
nydemos <- nydemos  %>%
  left_join(acs.vars[c('name', 'label')]
            ,by = c('variable' = 'name'))

nydemos

#' from the labels of the variable, we see that 001 is a total -- and 008 is a
#' subtotal of two subcategories of "two or more races"
#'
#' Having totals within a dataframe with the constiuent parts can be confusing
#' and pose issues for plotting and analysis.
nydemos %>%
  ggplot(aes(fill = label
             ,y = estimate
             ,x = name)
  ) +
  geom_col() +
  scale_y_continuous(
    name = 'population',
    labels = scales::label_comma()
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_fill_manual(
    name = NULL#'Race/ethnicity'
    ,values = c(visaux::jewel.pal(), 'grey35', 'black'
                ,'#f19df2', '#837ca6')
  ) +
  hrbrthemes::theme_ipsum(
    grid = 'Y'
  ) +
  labs(subtitle = 'Population by race/ethnicity in NYCs five boros')

# (what's the issue with this plot? How do we fix it?)




# (drop totals to avoid double-counting!)
nydemos %>%
  filter( !grepl('Total|Two or More Races: ',
                 label)) %>%
  ggplot(aes(fill = label
             ,y = estimate
             ,x = name)
  ) +
  geom_col()


