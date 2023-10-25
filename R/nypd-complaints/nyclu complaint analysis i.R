library(tidyverse)

rm(list = ls())

# load data ---------------------------------------------------------------

#' downloaded from https://github.com/new-york-civil-liberties-union/NYPD-Misconduct-Complaint-Database-Updated
#'
#' on 9/19/2023

# path to data
pth <-
  '~/R/local-data/nypd-complaints/CCRB Complaint Database Raw 04.28.2023/CCRB Complaint Database Raw 04.28.2023.csv'

pdcomplaints <- vroom::vroom(pth)

# transforms & cleans -------------------------------------------------------------


## clean the gender column ----------------------------------------------------

pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           case_when(
             ImpactedGender == 'Female/Woman' ~ 'Female'
             ,ImpactedGender == 'Male/Man' ~ 'Male'
             ,TRUE ~ ImpactedGender )
  )


## make colnames lowercase -------------------------------------------------

pdcomplaints <- pdcomplaints %>%
  rename_with( tolower )

## add a year column -------------------------------------------------------

library(lubridate)

pdcomplaints %>% select(matches('date'))
pdcomplaints <- pdcomplaints %>%
  mutate(incident.yr =
           lubridate::year(incidentdate)
           )

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

complaints.per.officer$n %>% summary()

#complaints.per.officer$n %>% quantile( )
c(0, .25, .5, .75, 1)
seq(0, 1, by= .25)

complaints.per.officer$n %>% quantile( seq(0,1, .01) )

complaints.per.officer %>%
  ggplot() +
  geom_histogram(
    aes(x = n)
    ,binwidth = 5
  )

plotly::ggplotly()


complaints.per.officer %>%
  filter(n >=
           quantile(complaints.per.officer$n
                    , .99 )
           )

pdcomplaints %>%
  select(matches('date|active|days'))

pdcomplaints %>% count(Status)

per.officer <- pdcomplaints %>%
  filter(Status == 'Active') %>%
  group_by(TaxID) %>%
  summarise( n.complaints = n()
            ,n.substantiated = sum(BoardCat == 'Substantiated')
            ) %>%
  arrange(desc(n.complaints))

per.officer
pdcomplaints$CurrentRank

pdcomplaints %>%
  filter(TaxID %in%
           per.officer$TaxID[1:5]
         ) %>%
  count(CurrentRank)

pdcomplaints %>%
  select(TaxID, DaysOnForce) %>%
  distinct() %>%
arrange(TaxID)
select(matches('date|active|days'))



# visuals and explorations ------------------------------------------------

#' some possibilites:
#'
#' how do misconducts correlate with outcomes?
#'
#' what is complaint when complainer is woman? -- and officer's location?

## how do misconducts correlate with outcomes? -----------------------------

pdcomplaints$incidentrank
pdcomplaints %>% count(penaltydesc)
pdcomplaints %>% count(penaltyrec)

# here is penalty outcome
pdcomplaints %>% count(penaltycat)

# misconducts or content of complaint
pdcomplaints %>% count(allegation) %>% arrange(desc(n))

pdcomplaints$incidentdate %>% range(na.rm = T)

pdcomplaints %>%
  filter(incident.yr > 2015) %>%
  count(allegation) %>%
  arrange(desc(n))

# When the allegation was physical force, what were the outcomes? were there
# other factors?

force.allegations <- pdcomplaints %>%
  filter( allegation ==
            'Physical force'
          ) %>%
  filter(incident.yr >= 2000)

# now let's plot
force.allegations %>%
  count(incident.yr, penaltycat) %>%
  ggplot(
    aes(
      y = factor(incident.yr)
      ,x = n
      ,fill = penaltycat
      ,color = penaltycat
    )) +
  geom_col()
