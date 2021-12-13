library(tidyverse)
library(igraph)
library(glue)

source('.Rprofile')
source('AoC functions.R')


download_advent(2021, 13)

empty_line <- input %>% 
  mutate(row = row_number()) %>%
  filter(value == '') %>%  
  pull(row)

coords <- input %>% 
  filter(row_number() < empty_line) %>% 
  separate(value, into = c('x', 'y')) %>% 
  mutate(across(everything(),
                as.numeric)) %>% 
  arrange(x, y)


folds <- input %>% 
  filter(row_number() > empty_line) %>% 
  mutate(direction = str_extract(value,
                                 '(x|y)(?==[0-9]+$)'),
         line = str_extract(value,
                                 '[0-9]+$') %>% 
           as.numeric()) 



line <- folds$line[1]
direction <- folds$direction[1]

coords <- coords %>% 
  mutate(y = case_when(y > line & direction == 'y' ~ y - (y - line)*2,
                       TRUE ~ y),
         x = case_when(x > line  & direction == 'x' ~ x - (x - line)*2,
                       TRUE ~ x)) %>% 
  arrange(x,y) %>% 
  distinct()

nrow(coords)


# Part 2 ------------------------------------------------------------------

coords <- input %>% 
  filter(row_number() < empty_line) %>% 
  separate(value, into = c('x', 'y')) %>% 
  mutate(across(everything(),
                as.numeric)) %>% 
  arrange(x, y)

for(i in 1:nrow(folds)){
  
  line <- folds$line[i]
  direction <- folds$direction[i]
  
  coords <- coords %>% 
    mutate(y = case_when(y > line & direction == 'y' ~ y - (y - line)*2,
                         TRUE ~ y),
           x = case_when(x > line  & direction == 'x' ~ x - (x - line)*2,
                         TRUE ~ x)) %>% 
    arrange(x,y) %>% 
    distinct()
  
}


coords %>% 
  mutate(y = 2.5-(y-2.5)) %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 5)


