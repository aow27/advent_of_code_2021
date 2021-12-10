library(tidyverse)

source('.Rprofile')
source('AoC functions.R')

download_advent(2021,10)

bracket_value = tibble(bracket = c(')', ']', '}', '>'),
               points = c(3, 57, 1197, 25137))

input_2 <- input %>% 
  mutate(detect = str_detect(value, '\\(\\)|\\{\\}|\\[\\]|\\<\\>'))
       
while(sum(input_2$detect) != 0) {  
  input_2 <- input_2 %>% 
  mutate(value = str_remove_all(string = value, pattern = regex("\\(\\)|\\{\\}|\\[\\]|\\<\\>")),
         detect = str_detect(value, '\\(\\)|\\{\\}|\\[\\]|\\<\\>')) 
}

input_2 %>% 
  mutate(no_end = str_detect(value, '[\\]\\}\\>\\)]')) %>% 
  filter(no_end == TRUE) %>% 
  mutate(bracket = str_extract(value, '[\\]\\}\\>\\)]')) %>% 
  left_join(bracket_value, by = 'bracket') %>% 
  summarise(sum(points))


# Part 2 ------------------------------------------------------------------


completed_point <- input_2 %>% 
  mutate(no_end = str_detect(value, '[\\]\\}\\>\\)]'),
         point = 0,
         length = nchar(value)) %>% 
  filter(no_end == F)

while(sum(completed_point$length != 0)){
  completed_point <- completed_point %>% 
    mutate(square = str_count(value, '\\($')*1,
           rect = str_count(value, '\\[$')*2,
           curly = str_count(value, '\\{$')*3,
           arrow = str_count(value, '\\<$')*4,
           
           point = ifelse(square + rect + curly + arrow > 0,
                          point*5 + square + rect + curly + arrow,
                          point),
           value = str_remove(value, '.$'),
           length = nchar(value))
}

completed_point %>% 
  summarise(median(point))

