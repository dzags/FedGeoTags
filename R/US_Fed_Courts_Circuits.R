#' This function will take either a vector of U.S. state/territories/Court District names or abbreviations and generate a new column identifying which of the 13 Federal Court Circuits they belong to (please note that the Federal Circuit is not included). 
#' The function will automatically make full names title case and make abbreviations uppercase. 
#' It attempts to capture all territories, including the various Pacific Islands, by identifying a range of ways they might be named. 
#' 
appeals_circuit <- function(x) {
  
  result <- vector('character')
  
  for (i in x) {
    
    if (nchar(as.character(i)) > 2) {
      
      i <- str_to_title(i)
      
      US_Federal_Courts_Circuit <- c()
      
      US_Federal_Courts_Circuit <- ifelse(i %in% c('District of Columbia', 'Washington DC', 'Washington D.C.'), 'District of Columbia Circuit',
                                          ifelse(i %in% c('Maine', 'District of Maine', 'Massachusetts', 'District of Massachusetts', 'New Hampshire', 'District of New Hampshire', 'Puerto Rico', 'District of Puerto Rico', 'Rhode Island', 'District of Rhode Island'), 'First Circuit',
                                                 ifelse(i %in% c('Connecticut', 'District of Connecticut', 'New York', 'Eastern District of New York', 'Northern District of New York', 'Southern District of New York', 'Western District of New York', 'Vermont', 'District of Vermont'), 'Second Circuit',
                                                        ifelse(i %in% c('Delaware', 'District of Delaware', 'District of New Jersey', 'Eastern District of Pennsylvania', 'Middle District of Pennsylvania', 'Western District of Pennsylvania', 'U.S. Virgin Islands', 'Virgin Islands', 'District of the Virgin Islands'), 'Third Circuit',
                                                               ifelse(i %in% c('Maryland', 'District of Maryland', 'North Carolina', 'Eastern District of North Carolina', 'Middle District of North Carolina', 'Western District of North Carolina', 'South Carolina', 'District of South Carolina', 'Virginia', 'Eastern District of Virginia', 'Western District of Virginia', 'West Virginia', 'Northern District of West Virginia', 'Southern District of West Virginia'), 'Fourth Circuit',
                                                                      ifelse(i %in% c('Louisiana', 'Eastern District of Louisiana', 'Middle District of Louisiana', 'Western District of Louisiana','Mississippi', 'Northern District of Mississippi', 'Southern District of Mississippi', 'Texas', 'Eastern District of Texas', 'Northern District of Texas', 'Southern District of Texas', 'Western District of Texas'), 'Fifth Circuit',
                                                                             ifelse(i %in% c('Kentucky', 'Eastern District of Kentucky', 'Western District of Kentucky', 'Michigan', 'Eastern District of Michigan', 'Western District of Michigan', 'Ohio', 'Northern District of Ohio', 'Southern District of Ohio', 'Tennessee', 'Eastern District of Tennessee', 'Western District of Tennessee', 'Middle District of Tennessee'), 'Sixth Circuit',
                                                                                    ifelse(i %in% c('Illinois', 'Central District of Illinois', 'Northern District of Illinois', 'Southern District of Illinois', 'Indiana', 'Northern District of Indiana', 'Southern District of Indiana', 'Wisconsin', 'Eastern District of Wisconsin', 'Western District of Wisconsin'), 'Seventh Circuit',
                                                                                           ifelse(i %in% c('Arkansas', 'Eastern District of Arkansas', 'Western District of Arkansas', 'Iowa', 'Northern District of Iowa', 'Southern District of Iowa', 'Minnesota', 'District of Minnesota', 'Missouri', 'Eastern District of Missouri', 'Western District of Missouri', 'Nebraska', 'District of Nebraska', 'North Dakota', 'District of North Dakota', 'South Dakota', 'District of South Dakota'), 'Eighth Circuit',
                                                                                                  ifelse(i %in% c('Alaska', 'District of Alaska', 'Arizona', 'District of Arizona', 'California', 'Central District of California', 'Eastern District of California', 'Northern District of California', 'Southern District of California', 'Guam', 'District of Guam', 'Hawaii', 'District of Hawaii', 'Idaho', 'District of Idaho', 'Montana', 'District of Montana', 'Nevada', 'District of Nevada', 'Northern Mariana Islands', 'District of the Northern Mariana Islands', 'Oregon', 'District of Oregon', 'Washington', 'Eastern District of Washington', 'Western District of Washington'), 'Ninth Circuit',
                                                                                                         ifelse(i %in% c('Colorado', 'District of Colorado', 'Kansas', 'District of Kansas', 'New Mexico', 'District of New Mexico', 'Oklahoma', 'Eastern District of Oklahoma', 'Northern District of Oklahoma', 'Western District of Oklahoma', 'Utah', 'District of Utah', 'Wyoming', 'District of Wyoming'), 'Tenth Circuit',
                                                                                                                ifelse(i %in% c('Alabama', 'Middle District of Alabama', 'Northern District of Alabama', 'Southern District of Alabama', 'Florida', 'Middle District of Florida', 'Northern District of Florida', 'Southern District of Florida', 'Georgia', 'Middle District of Georgia', 'Northern District of Georgia', 'Southern District of Georgia'), 'Eleventh Circuit', NA))))))))))))
      
      result[i] <- US_Federal_Courts_Circuit
      
      
    } else {
      
      i <- toupper(i)
      
      US_Federal_Courts_Circuit <- ifelse(i %in% c('DC'), 'District of Columbia Circuit',
                                          ifelse(i %in% c('ME', 'MA', 'NH', 'PR', 'RI'), 'First Circuit',
                                                 ifelse(i %in% c('CT', 'NY', 'VT'), 'Second Circuit',
                                                        ifelse(i %in% c('DE', 'NJ', 'PA', 'VI'), 'Third Circuit',
                                                               ifelse(i %in% c('MD', 'NC', 'SC', 'VA', 'WV'), 'Fourth Circuit',
                                                                      ifelse(i %in% c('LA', 'MS', 'TX'), 'Fifth Circuit',
                                                                             ifelse(i %in% c('KY', 'MI', 'OH', 'TN'), 'Sixth Circuit',
                                                                                    ifelse(i %in% c('IL', 'IN', 'WI'), 'Seventh Circuit',
                                                                                           ifelse(i %in% c('AR', 'IA', 'MN', 'MO', 'NE', 'ND', 'SD'), 'Eighth Circuit',
                                                                                                  ifelse(i %in% c('AK', 'AZ', 'CA', 'GU', 'HI', 'ID', 'MT', 'NV', 'MP', 'OR', 'WA'), 'Ninth Circuit',
                                                                                                         ifelse(i %in% c('CO', 'KS', 'NM', 'OK', 'UT', 'WY'), 'Tenth Circuit',
                                                                                                                ifelse(i %in% c('AL', 'FL', 'GA'), 'Eleventh Circuit', NA))))))))))))
      result[i] <- US_Federal_Courts_Circuit
      
    }
  }
  
  result
}
