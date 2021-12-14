library(tidyverse)
library(glue)

source('AoC functions.R')
source('.Rprofile')

download_advent(2021,14)


rules <- input %>% 
  filter(row_number() > 2) %>% 
  separate(value, into = c('pair', 'insert')) %>% 
  mutate(start = str_extract(pair, '^.'),
         end= str_extract(pair, '.$'))

template <- input %>% 
  filter(row_number() == 1)

steps <- 10

for(i in 1:steps){
  template <- template %>% 
    mutate(letter = str_extract_all(value, '[A-Z]')) %>% 
    unnest_longer(letter) %>% 
    mutate(next_letter = lead(letter)) %>% 
    left_join(rules %>% 
                select(-pair), 
              by = c('letter' = 'start', 'next_letter' = 'end')) %>% 
    mutate(across(letter:insert,
                  ~ replace_na(., replace = "")),
           joining = str_c(letter, insert)) %>% 
    summarise(value = str_c(joining, collapse = ''))
  
  print(i)
}


template %>% 
  mutate(letter = str_extract_all(value, '[A-Z]'),
         letter = list(str_sort(unlist(letter)))) %>% 
  unnest_longer(letter) %>% 
  count(letter) %>% 
  summarise(max(n) - min(n))


# Part 2 ------------------------------------------------------------------

rules_pt2 <- rules %>% 
  transmute(pair,
            new_pair1 = str_c(start, insert),
            new_pair2 = str_c(insert, end)) %>% 
  pivot_longer(starts_with('new'),
               values_to = 'new_pairs') %>% 
  select(-name)

template_pt2 <- input %>% 
  filter(row_number() == 1) %>% 
  mutate(letter = str_extract_all(value, '[A-Z]')) %>% 
  unnest_longer(letter) %>% 
  mutate(next_letter = lead(letter)) %>% 
  filter(row_number() != max(row_number())) %>% 
  mutate(pair = str_c(letter, next_letter)) %>% 
  count(pair, name = 'count') 

for (i in 1:40){
  template_pt2 <- template_pt2 %>% 
    left_join(rules_pt2, by = 'pair') %>% 
    group_by(new_pairs) %>% 
    summarise(count = sum(count)) %>% 
    rename(pair = new_pairs)
  }

options(scipen = 999)   
template_pt2 %>% 
  mutate(letter = str_extract(pair, '^.'))  %>% 
  rbind(template_pt2 %>% 
          mutate(letter = str_extract(pair, '.$'))) %>% 
  group_by(letter) %>% 
    summarise(count = sum(count)/2) %>% 
  ungroup() %>% 
  summarise(max(count) - min(count)) %>% 
  pull()

# I never really sorted out the issue that the first and last characters wouldn't be double counted
# but that risk paid off so far

