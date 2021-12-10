library(tidyverse)

source('.Rprofile')
source('AoC functions.R')

download_advent(2021,10)

input <- tibble(value = read_lines('[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]'))

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


bracket_value = tibble(bracket = c(')', ']', '}', '>'),
                       points = c(1, 2, 3, 4))

completed_point <- input_2 %>% 
  mutate(no_end = str_detect(value, '[\\]\\}\\>\\)]')) %>% 
  filter(no_end == F) %>% 
  mutate(square = str_count(value, '\\('),
         rect = str_count(value, '\\['),
         curly = str_count(value, '\\{'),
         arrow = str_count(value, '\\<'),
         
         points = ((square*5 + rect)*5 + curly)*5 + arrow) 

completed_point %>% 
  summarise(sum(points))

