# A function that will automatically download the input for a given day and year and input it into the 
# folder data, and then creates a variable input with the global environment

# Requires knowing the AoC session cookie, which is typically found in the storage section of a web inspector
# I've then saved this in my Rprofile file which isn't publically shared

download_advent <- function(year,
                           day){
  if(!exists('cookie')){
    message("The cookie input isn't loaded, please make sure you have a cookie variable with your AoC session cookie")
  
    break
  }
  
  if(file.exists(glue::glue('data/input{year}_{day}.txt'))){
    message('This file has already been downloaded from the AoC servers - no need to download again')
    
    rm(cookie,
       envir = .GlobalEnv)
    
    input <<- tibble(read_lines(glue::glue('data/input{year}_{day}.txt')))
  } else {
    glue::glue('curl "https://adventofcode.com/{year}/day/{day}/input" -H "cookie: session={cookie}" -o "data/input{year}_{day}.txt" 2>/dev/null') %>% 
      system()
    
    message(glue::glue('A file at "data/input{year}_{day}.txt" has now been created - enjoy!'))
    rm(cookie,
       .GlobalEnv)
    
    input <<- tibble(read_lines(glue::glue('data/input{year}_{day}.txt')))
    
  }
}