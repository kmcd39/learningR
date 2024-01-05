library(tidyverse)

rm(list = ls())


# pull a dataset ----------------------------------------------------------

#' this is bundled with the `tidyr` package, part of the tidyverse:
pop <- tidyr::world_bank_pop

# we can look at the documentation for the dataset the same way we look at
# documentation for a function:
?tidyr::world_bank_pop

# Right now the dataset is long by country and indicator, and wide by year.

#' what if we want to reverse that? Get the table long by country and year, but
#' wide by indicator?
#'
#' That would make time-series analysis in R easier.
#'
#' We can pivot the table longer, by all year columns, using the special "any
#' number" regex character:

pop

pop %>%
  pivot_longer(
    matches('\\d')
    ,names_to = 'year'
  )
