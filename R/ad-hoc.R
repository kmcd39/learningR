install.packages('tidyverse')


library(tidyverse)


?tidycensus::load_variables()


acs.vars <-
  tidycensus::load_variables(
   year = 2021
  ,'acs5'
)


acs.vars %>%
  mutate(label =
           gsub('!!', ' ', label)) %>%
  write.csv('acs-vars.csv')



rentburden <-
  tidycensus::get_acs(
      geography = 'county'
      ,table = 'B25070'
      ,year = 2021
      ,state = 'NY'
  )

most.burdened <- rentburden %>%
  mutate(var =
           censusrx::extract.acs.var(variable)) %>%
  mutate(rentburden.status =
           if_else(var %in%
                     7:10
                   ,T
                   ,F)) %>%
  filter(var != 1) %>%
  group_by(GEOID, NAME,
           rentburden.status) %>%
  summarise(n.burdened.hh =
              sum(estimate)) %>%
  filter(rentburden.status) %>%
  arrange(desc(n.burdened.hh))

ordr <- most.burdened$NAME


most.burdened %>%
  head(10) %>%
  ungroup() %>%
  mutate(NAME = factor(NAME
                       ,levels = ordr)) %>%
  ggplot(
    aes(y = NAME
        , x = n.burdened.hh
        ,fill = n.burdened.hh)
  ) +
  geom_col()





# bbeep -------------------------------------------------------------------

tidyr::world_bank_pop
