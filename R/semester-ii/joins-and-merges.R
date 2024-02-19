library(tidyverse)


# resources ---------------------------------------------------------------

#' also see the Joins section in the R textbook!
#'
#' https://r4ds.hadley.nz/joins.html


# read OECD data ----------------------------------------------------------

ddir <- '~/R/local-data/OECD/'

fns <- ddir %>% list.files(pattern = 'csv$')

gdp <- paste0(ddir, fns[grepl('per capita gdp', fns)]) %>%
  read.csv() %>% tibble()

lxp <- paste0(ddir, fns[grepl('life exp', fns)]) %>%
  read.csv() %>% tibble()


# peeks -------------------------------------------------------------------


## peek and trim useless colms ---------------------------------------------

lxp %>%
  select( where( ~length(unique(.x)) == 1)) %>%
  glimpse()

lxp <- lxp %>%
  select( where( ~length(unique(.x)) > 1))


gdp %>%
  select( where( ~length(unique(.x)) == 1)) %>%
  glimpse()

gdp <- gdp %>%
  select( where( ~length(unique(.x)) > 1))

lxp %>% glimpse()
gdp %>% glimpse()


# plot theme --------------------------------------------------------------

plot.theme <- function() {

  hrbrthemes::theme_ipsum()

}
# life expectancy alone ---------------------------------------------------

lxp %>% count(REF_AREA, Reference.area)

lxp %>%
  ggplot(
    aes(x = TIME_PERIOD
        , y = OBS_VALUE
        ,group = REF_AREA
        ,label = Reference.area)
  ) +
  geom_path() +
  scale_x_continuous('Year') +
  scale_y_continuous('Life expectancy') +
  plot.theme()

plotly::ggplotly()



## gdp alone ---------------------------------------------------------------

gdp %>%
  ggplot(
    aes(x = TIME_PERIOD
        , y = OBS_VALUE
        ,group = REF_AREA
        ,label = Reference.area)
  ) +
  geom_path() +
  scale_x_continuous('Year') +
  scale_y_continuous(
    'GDP per capita'
    ,labels = scales::label_number()) +
  plot.theme()

plotly::ggplotly()


# combining the datasets! ------------------------------------------------------------------

# what do we notice about the two datasets? (think about the column names, the
# data structure.)
lxp %>% glimpse()
gdp %>% glimpse()

# ...


#' They have identical column names.
#'
#' They're both long by country and year.
#'
#' The other variable (gdp per capita or life expectancy) is always called
#' "OBS_VALUE".


# row binding -------------------------------------------------------------

#' We can combine in to ways... because the data has such idenitical structure,
#' we can add a new column to each that indicate the other variable, and then
#' ROW BIND, which basically stacks the datasets on top of one another.

lxp.tmp <- lxp %>% mutate(var = 'life expectancy')
gdp.tmp <- gdp %>% mutate(var = 'GDP per capita')

lxp.gdp <- lxp.tmp %>%
  rbind(gdp.tmp)

# notice the number of rows in the new table is equal to the number of rows in
# the previous two combined.
lxp.gdp %>% nrow()
lxp.tmp %>% nrow()
gdp.tmp %>% nrow()

# what is the data structure of the new table?
lxp.gdp

# merging -----------------------------------------------------------------

#' more frequently, the data will have some matching columns, but not ALL
#' matching columns as above.
#'
#' Let's simulate that by renaming the OBSVAALUE column for each.
#'

lxp.tmp <- lxp %>% rename('life.exp' = OBS_VALUE )
gdp.tmp <- gdp %>% rename(gdp.per.cap = OBS_VALUE )

lxp.tmp
gdp.tmp

#' to merge... there are some mistakes that are easy to make but also relatively
#' easy to avoid!
#'
#' We typically need at least one column where we want to join by common
#' values...
#'
#' In this example, we'll expect at least two: we want to join by COUNTRY and
#' YEAR.
#'
#' Joins can be one-to-one or many-to-many. For example, if we're joining a
#' table that has 1 row/country to one with 5 rows/country, by only the one
#' country column, we'll have a new table that's bigger than the first, with
#' rows from the first table repeated for every match in the second table.
#'
#' The can be good deliberate sometimes but unexpected other times! (as with
#' many things, it's good to make sure everything went as expected even if you
#' get an error!)

# so our first join attempt:
gdp.lxp.tmp <- lxp.tmp %>%
  left_join(gdp.tmp)
# when we don't specify which columns to join by, it'll try all of the coluns in
# common....

# after we tried to join in GDP, we have 1562 NAs for it!!!! What might be going on?
gdp.lxp.tmp$gdp.per.cap %>% summary()

# ...

#' Maybe it'll go better if we don't try to merge by the OBS_STATUS columns!!
#' Notice these are marked slightly differently in the two tables! And we
#' wouldn't want to join based on estimate status anyway..
lxp %>% count(OBS_STATUS, Observation.status)
gdp %>% count(OBS_STATUS, Observation.status)

# for now, let's just drop from both tables.
lxp.tmp <- lxp.tmp %>% select(-c(OBS_STATUS, Observation.status))
gdp.tmp <- gdp.tmp %>% select(-c(OBS_STATUS, Observation.status))

gdp.lxp.tmp <- lxp.tmp %>%
  left_join(gdp.tmp)

# there are still some NAs, but we did start with a different selection of
# countries..
gdp.lxp.tmp$gdp.per.cap %>% summary()

# we can look at countries missing from the merged data...
gdp.lxp.tmp %>%
  filter(is.na(gdp.per.cap)) %>%
  count(REF_AREA, Reference.area) %>%
  arrange(REF_AREA)

# ...and those that existed in one of our datasets but not the other
lxp.tmp %>%
  filter(! REF_AREA %in%
           gdp.tmp$REF_AREA) %>%
  count(REF_AREA, Reference.area) %>%
  arrange(REF_AREA)

# We also know that some years are missing for GDP, which accounts for the
# remaining NAs.




# analyzing together! -----------------------------------------------------

gdp.lxp.tmp %>%
  filter(TIME_PERIOD == 2021) %>%
  ggplot(
    aes(x = gdp.per.cap
        ,y = life.exp)
  ) +
  geom_point() +
  geom_smooth() +
  plot.theme()




## connected time series scatter plot... -----------------------------------


gdp.lxp.tmp %>%
  ggplot(
    aes(x = gdp.per.cap
        ,y = life.exp
        ,group = Reference.area)
  ) +
  geom_line()

# how can we clean up?


gdp.lxp.tmp %>% count(REF_AREA, Reference.area)

gdp.lxp.tmp %>%
  filter(TIME_PERIOD == 2022) %>%
  arrange(desc(gdp.per.cap))

# trim Luxembourg and Ireland (a quick google search shows some articles thay
# say Ireland GDP per cap is misleadingly inflated, and Luxembourg is a very
# small, very wealthy country.)
outlier.countries <- gdp.lxp.tmp %>%
  filter(TIME_PERIOD == 2021) %>%
  filter(gdp.per.cap > 90e3)

# trim those (what does the ! mean?)
tmp <- gdp.lxp.tmp %>%
  filter(! REF_AREA %in%
           outlier.countries$REF_AREA)

tmp

# also add a US identifier! let's see how our own country is doing.
tmp <- tmp %>%
  mutate(is.US = grepl('United States', Reference.area))

tmp %>%
  filter(TIME_PERIOD <= 2021) %>%
  arrange(REF_AREA, TIME_PERIOD
          ) %>%
  ggplot(
    aes(x = gdp.per.cap
        ,y = life.exp
        ,group = Reference.area
        ,alpha = is.US
        ,label = TIME_PERIOD
    )
  ) +
  geom_path(

  ) +
  geom_point(
    data = filter(tmp,
                  TIME_PERIOD == 2021)
  )

plotly::ggplotly()
xx
