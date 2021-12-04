library(tidyverse)

source('AoC functions.R')

download_advent(2021,
                4)


call_order <- str_split(input$value[1],
          ',') %>% 
  unlist() %>% 
  as.numeric()


tidy_bingo <- input %>% 
  filter(row_number() > 2,
         value != "") %>% 
  mutate(set = (row_number() - 1)%/% 5 + 1,
         value = str_trim(value)) %>% 
  
  group_by(set) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  
  separate(value, str_c('col_', 1:5)) %>% 
  mutate(across(col_1:col_5,
               as.numeric)) %>% 
  
  pivot_longer(starts_with('col')) %>% 
  mutate(col = str_sub(name, -1, -1) %>% 
           as.numeric())

for(i in 1:length(call_order)){
  
  print(i)
  
  check <<- call_order[1:i]
  
  result <<- tidy_bingo %>% 
    mutate(hit = value %in% check) %>% 
    group_by(set, row) %>% 
    mutate(row_check = sum(hit)  == 5) %>% 
  group_by(set, col) %>% 
    mutate(col_check = sum(hit)  == 5) %>% 
    ungroup() %>% 
    distinct(set, row_check, col_check)
  
  if(sum(result$row_check + result$col_check) != 0) break
}
 
winning_set <- result %>% 
  filter(row_check == T | col_check== T) %>% 
  pull(set)

tidy_bingo %>% 
  filter(set == winning_set,
         !value %in% check) %>% 
  pull(value) %>% 
  sum()*check[i]


# Part 2 ------------------------------------------------------------------

for(i in 1:length(call_order)){
  
  print(i)
  
  check <<- call_order[1:i]
  
  result <<- tidy_bingo %>%  
    mutate(hit = value %in% check) %>% 
    group_by(set, row) %>% 
    mutate(row_check = sum(hit)  == 5) %>% 
    group_by(set, col) %>% 
    mutate(col_check = sum(hit)  == 5) %>% 
    group_by(set) %>% 
    summarise(row_check = max(row_check), 
              col_check = max(col_check))%>% 
    mutate(either = ifelse(row_check + col_check == 0,
                           F,
                           T))
  
  winners <<- result %>% 
    filter(either == T) %>% 
    pull(set)
    
  if(length(winners) == max(result$set) - 1) {
    losing_set <<- result %>% 
      filter(either == F) %>% 
      pull(set)
  }
  
  if(length(winners) == max(result$set)) {break}
}


tidy_bingo %>% 
  filter(set == losing_set,
         !value %in% check) %>% 
  pull(value) %>% 
  sum()*check[i]
