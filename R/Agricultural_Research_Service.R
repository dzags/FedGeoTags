#' This function will take either a vector of U.S. state/territories/Court District names or abbreviations and generate a new column identifying which of the 13 Federal Court Circuits they belong to (please note that the Federal Circuit is not included). 
#' The function will automatically make full names title case and make abbreviations uppercase. 
#' It attempts to capture all territories, including the various Pacific Islands, by identifying a range of ways they might be named. 
#' 
ARS <- function(x) {
  
  result <- vector('character')
  
  for (i in x) {
    
    if (nchar(as.character(i)) > 2) {
      
      i <- str_to_title(i)
      
      ARS <- c()
      
      ARS <- 
        
      result[i] <- ARS
      
      
    } else {
      
      i <- toupper(i)
      
      ARS <-                    
        
      result[i] <- ARS
      
    }
  }
  
  result
}
