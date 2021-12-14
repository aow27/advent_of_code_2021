library(tidyverse)
library(tictoc)

source('AoC functions.R')

download_advent(2021,
                9)

df <- input  %>% 
  separate(value, sep = '(?<=[0-9])', into = paste0('value_', 1:nchar(input$value[1]))) %>% 
  mutate(across(everything(),
                as.numeric))


check_around <- function(df,
                         row,
                         col,
                         combined = T){
  
  row_max = nrow(df)
  col_max = ncol(df)
  
  ## Check the rows
  if(row == 1){
    around_row <- df[[row + 1, col]]
  } else if(row == row_max){
    around_row <- df[[row - 1, col]]
  } else{
    around_row <- c(df[[row - 1, col]], df[[row + 1, col]])
  }
  
  ## Check the rows
  if(col == 1){
    around_col <- c(df[[row, col + 1]])
  } else if(col == col_max){
    around_col <- c(df[[row, col - 1]])
  } else{
    around_col <- c(df[[row, col - 1]], df[[row, col + 1]])
  }
  
  output <- (df[[row, col]] >= around_row |
      df[[row, col]] >= around_col) %>% 
    sum() == 0
  
  if(!combined) {
    row_check <-c(row = around_row[df[[row, col]] < around_row])
    col_check <- c(col = around_col[df[[row, col]] < around_col])
  }
<<<<<<< HEAD
=======
  
  output
>>>>>>> main
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


# Part 2 ------------------------------------------------------------------
lowest_points <- checks %>% 
  filter(check == T) 

row <- lowest_points[[1,'row']]
col <- lowest_points[[1,'col']]

check_around(df, row, col, combined = F)
