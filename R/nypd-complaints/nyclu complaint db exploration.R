library(tidyverse)


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

# data exploration --------------------------------------------------------

pdcomplaints <- pdcomplaints %>% tibble()

pdcomplaints %>% colnames()
pdcomplaints %>% count(PenaltyCat)

pdcomplaints %>% count(OfficerRace)

# initial analysis --------------------------------------------------------


# visualizations ----------------------------------------------------------


