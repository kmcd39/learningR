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

## some cleans -------------------------------------------------------------

# clean the gender column
pdcomplaints <- pdcomplaints %>%
  mutate(ImpactedGender =
           case_when(
             ImpactedGender == 'Female/Woman' ~ 'Female'
             ,ImpactedGender == 'Male/Man' ~ 'Male'
             ,TRUE ~ ImpactedGender )
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

