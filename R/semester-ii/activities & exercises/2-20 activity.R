library(tidyverse)


pth <- 'local-data/processed-datasets/EIA-monthly-energy-review-w-indices.csv'

eia <- read.csv(pth) %>% tibble()

eia
