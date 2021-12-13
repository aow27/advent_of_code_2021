library(tidyverse)
library(igraph)
library(glue)

source('.Rprofile')
source('AoC functions.R')


download_advent(2021, 12)


input <- tibble(value = read_lines('start-A
start-b
A-c
A-b
b-d
A-end
b-end'))

input <- tibble(value = read_lines('fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW'))
 
input <- input %>% 
  separate(value, into = c('start_node', 'last_node')) %>% 
  rbind(input %>%  
          separate(value, into = c('start_node', 'last_node')) %>% 
          rename(last_node = start_node,
                 start_node = last_node)) %>%
  filter(start_node != 'end',
         last_node != 'start') %>% 
  distinct() %>% 
  arrange(start_node, last_node)



# -------------------------------------------------------------------------

track <- input %>% 
  filter(start_node == 'start') %>% 
  mutate(path = last_node,
         end = F,
         small_repeat = F,
         finished = 0) %>% 
  select(-start_node)



while(sum(track$finished) != nrow(track)){
  
  track <- track %>% 
    rename(first_node = last_node) %>% 
    left_join(input,
              by = c('first_node' = 'start_node')) %>% 
  
  mutate(last_node = replace_na(last_node, ''),
         path = str_c(path, last_node,sep = ','),
         small_repeat = str_detect(path, '([a-z][a-z]).*\\1'),
         end = case_when(end == TRUE ~ TRUE,
                         last_node == 'end' ~ TRUE,
                         TRUE ~ FALSE),
         finished = ifelse(small_repeat == T |
                             end == T,
                           1,
                           0)) %>% 
    select(-first_node) %>% 
    filter(!(small_repeat == T & end == T),
           small_repeat == F)
  

}


nrow(track)


# part 2 ------------------------------------------------------------------
track <- input %>% 
  filter(start_node == 'start') %>% 
  mutate(path = last_node,
         end = F,
         small_repeat = F,
         finished = 0) %>% 
  select(-start_node)

while(sum(track$finished) != nrow(track)){
  
  track <- track %>% 
    rename(first_node = last_node) %>% 
    left_join(input,
              by = c('first_node' = 'start_node')) %>% 
    
    mutate(last_node = replace_na(last_node, ''),
           path = str_c(path, last_node,sep = ','),
           small_repeat = str_detect(path, '([a-z][a-z],).*\\1.*\\1'),
           small_count = str_count(path, '([a-z][a-z])(?=.*,\\1(?=,|$))'),
           end = case_when(end == TRUE ~ TRUE,
                           last_node == 'end' ~ TRUE,
                           TRUE ~ FALSE),
           finished = ifelse(small_repeat == T |
                               end == T,
                             1,
                             0)) %>% 
    select(-first_node) %>% 
    filter(
      small_repeat == F
           ,small_count <= 1
           )
  
  
}

nrow(track)