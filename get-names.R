# get names
library(tidyverse)
library(babynames)

names <- 
babynames %>% 
  filter(year >= 1990, year < 2000) %>% 
  count(sex, name, wt = n, sort = T) %>%
  group_by(sex) %>% 
  top_n(16)

names %>% 
  ungroup() %>% 
  arrange(desc(sex), desc(n)) %>% 
  View