library(tidyverse)

# load OECCD time use data ------------------------------------------------

#' downloaded from
#'
#'  https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CWell-being%20and%20beyond%20GDP%23SOC_WEL%23&pg=0&fc=Topic&bp=true&snb=2&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TIME_USE%40DF_TIME_USE&df[ag]=OECD.WISE.INE&df[vs]=1.0&dq=.._T&ly[rw]=REF_AREA&ly[cl]=MEASURE&to[TIME]=false
pth <- "~/R/learningR/local-data/oecd/OECD.WISE.INE,DSD_TIME_USE@DF_TIME_USE,1.0+..F+M.csv"
file.exists(pth)
timeuse <- read.csv(pth)
timeuse <- tibble(timeuse)

timeuse
timeuse %>% glimpse()

# questions using this data: ----------------------------------------------

#' Will hypothesizes that Personal Care will be relatively stable across all the
#' different categories and groups.
#'
#' Hwayong is interested in differences between Leisure and Personal care (as
#' well as unpaid work) by gender.
#'
#'



# adding continent identifier to this table -------------------------------

# find a country2continent list someone else already made
country2continent <-
  read.csv('https://raw.githubusercontent.com/datawookie/data-diaspora/master/spatial/country-continent-codes.csv'
           ,skip =1) %>%
  tibble()

country2continent

# join and drop extra columns
timeuse.cont <- timeuse %>%
  left_join(country2continent[c('continent', 'iso3')]
            , by = c('REF_AREA' = 'iso3')
            ) %>%
  select(REF_AREA, country.name = Reference.area,
         Measure, SEX, OBS_VALUE, continent)

timeuse.cont %>% View()
# trim some columns -------------------------------------------------------

glimpse(timeuse)

timeuse <- timeuse %>%
   select(Reference.area,
           Measure, SEX, OBS_VALUE) %>%
  rename_with(tolower)

timeuse %>% count(reference.area)

# how to get the average by time-use column? ----------------------------------------

timeuse

timeuse %>%
  count(measure)

timeuse %>%
  group_by(measure) %>%
  summarise( mean(obs_value) )

# summarize collapses by group
timeuse %>%
  group_by(measure) %>%
  summarise(mean(obs_value))

# mutate adds new columns
timeuse %>%
  group_by(measure) %>%
  mutate(mean = mean(obs_value)
         ,median = median(obs_value)
         )


# what if we want the average by time-use category AND gender? (collapse only by
# country.)
timeuse %>%
  group_by(measure, sex) %>%
  summarise(mean(obs_value))
