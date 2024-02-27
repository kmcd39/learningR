library(tidyverse)


#pth <- 'local-data/processed-datasets/EIA-monthly-energy-review-w-indices.csv'
pth <- 'https://raw.githubusercontent.com/kmcd39/learningR/main/local-data/processed-datasets/EIA-monthly-energy-review-w-indices.csv'
eia <- read.csv(pth) %>% tibble()

eia %>% count(description)

eia %>% glimpse()

eia %>% count(description, lbl, is.tot)

eia %>%
  filter(!is.tot) %>%
  ggplot(
    aes(x = year
        ,y = value
        ,color = lbl)
  ) +
  geom_line() +
  scale_color_brewer(
  ) +
  theme_dark()



eia %>%
  filter(!is.tot) %>%
  ggplot(
    aes(x = year
        ,y = value
        ,color = lbl
        #,fill = lbl
        )
  ) +
  geom_area() +
  scale_color_brewer(
    aesthetics = c('color', 'fill')
  ) +
  theme_dark()
