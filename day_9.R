library(tidyverse)

input <- tibble(value = read_lines('Data/input9.txt'))

df <- input  %>% 
  separate(value, sep = '(?<=[0-9])', into = paste0('value_', 1:nchar(input$value[1]))) %>% 
  mutate(across(everything(),
                as.numeric))


check_around <- function(df,
                         row,
                         col){
  
  row_max = nrow(df)
  col_max = ncol(df)
  
  ## Check the rows
  if(row == 1){
    around <- df[[row + 1, col]]
  } else if(row == row_max){
    around <- df[[row - 1, col]]
  } else{
    around <- c(df[[row - 1, col]], df[[row + 1, col]])
  }
  
  ## Check the rows
  if(col == 1){
    around <- c(around, df[[row, col + 1]])
  } else if(col == col_max){
    around <- c(around, df[[row, col - 1]])
  } else{
    around <- c(around, df[[row, col - 1]], df[[row, col + 1]])
  }
  
  (df[[row, col]] >= around) %>% 
    sum() == 0
}


checks <- tibble(col = rep(1:ncol(df), each = nrow(df)),
       row = rep(1:nrow(df), ncol(df))) %>% 
  rowwise() %>% 
  mutate(value = df[[row, col]],
         check = check_around(df, row, col)) %>% 
  ungroup()

checks %>% 
  filter(check == T) %>% 
  summarise(sum(value + 1))
  
