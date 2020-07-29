#' This function will take either a vector of U.S. state/territories names or abbreviations and generate a new column identifying which of the 8 Bureau of Economic Analysis Regions the statesbelongs to. 
#' The function will automatically make state names title case and make abbreviations uppercase. 
#' Note: OMB's division of the United States is the standard for many other Federal agencies such as EPA and FEMA.


BEA <- function(x) {
  
  result <- vector('character')
  
  for (i in x) {
    
    if (nchar(as.character(i)) > 2) {
      
      i <- stringr::str_to_title(i)
      
      
      BEA <- ifelse(i %in% c('Maine', 'New Hampshire', 'Vermont', 'Connecticut', 'Rhode Island', 'Maine'), 'New England',
                    ifelse(i %in% c('New York', 'Pennsylvania', 'Maryland', 'Delaware', 'New Jersey', 'District of Columbia', 'Washington D.C.'), 'Mideast',
                           ifelse(i %in% c('Wisconsin', 'Illinois','Michigan', 'Indiana', 'Ohio'), 'Great Lakes',
                                  ifelse(i %in% c('North Dakota', 'Minnesota', 'South Dakota', 'Iowa', 'Nebraska', 'Kansas', 'Missouri'), 'Plains',
                                         ifelse(i %in% c('West Virginia', 'Kentucky', 'Virginia', 'Arkansas', 'Tennessee', 'North Carolina', 'Lousiana', 'Mississippi', 'Alabama', 'Georgia, South Carolina', 'Florida'), 'Southeast',
                                                ifelse(i %in% c('Arizona', 'New Mexico', 'Oklahoma', 'Texas'), 'Southwest',
                                                       ifelse(i %in% c('Montana', 'Idaho', 'Wyoming', 'Utah', 'Colorado'), 'Rocky Mountains',
                                                              ifelse(i %in% c('Washington', 'Oregon', 'California', 'Nevada', 'Hawaii', 'Alaska'), 'Far West', NA))))))))
      
      result[i] <- BEA
      
      
      
    } else {
      
      i <- toupper(i)
      
      BEA <- ifelse(i %in% c('ME', 'NH', 'VT', 'CT', 'RI', 'MA'), 'New England',
                    ifelse(i %in% c('NY', 'PA', 'MD', 'DE', 'NJ', 'DC'), 'Mideast',
                           ifelse(i %in% c('WI', 'IL','MI', 'IN', 'OH'), 'Great Lakes',
                                  ifelse(i %in% c('ND', 'MN', 'SD', 'IA', 'NE', 'KS', 'MO'), 'Plains',
                                         ifelse(i %in% c('WV', 'KY', 'VA', 'AR', 'TN', 'NC', 'LA', 'MS', 'AL', 'GA, SC', 'FL'), 'Southeast',
                                                ifelse(i %in% c('AZ', 'NM', 'OK', 'TX'), 'Southwest',
                                                       ifelse(i %in% c('MT', 'ID', 'WY', 'UT', 'CO'), 'Rocky Mountains',
                                                              ifelse(i %in% c('WA', 'OR', 'CA', 'NV', 'HI', 'AK'), 'Far West', NA))))))))
      
      result[i] <- BEA
     
    }
    
  }
  result
}

