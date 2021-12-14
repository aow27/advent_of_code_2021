library(tidyverse)
library(glue)

input <- tibble(value = read_lines('Data/input14.txt'))

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

  rules %>% 
    mutate(new_pair1 = str_c(start, insert),
           new_pair2 = str_c(insert, end))
  
 
  template %>% 
    mutate(letter = str_extract_all(value, '[A-Z]')) %>% 
    unnest_longer(letter) %>% 
    mutate(next_letter = lead(letter)) %>% 
    filter(row_number() != max(row_number())) %>% 
    mutate(pair = str_c(letter, next_letter)) %>% 
    count(pair)
  
  
  # Looking at counting pairs
