#' This function will take either a vector of U.S. state/territories names or abbreviations and generate a new column identifying which of the 10 OMB Regions the state/territory belongs to. 
#' The function will automatically make state/territory names title case and make abbreviations uppercase. 
#' It attempts to capture all territories, including the various Pacific Islands, by identifying a range of ways they might be named. 
#' Note: OMB's division of the United States is the standard for many other Federal agencies such as EPA and FEMA.

OMB <- function(x) {
  
  result <- vector('character')
  
  for (i in x) {
  
    
    if (nchar(as.character(i)) > 2) {
      
      i <- stringr::str_to_title(i)
      
      
      OMB <- ifelse(i %in% c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont') ,'Region 1', 
                     ifelse(i %in% c('New Jersey', 'New York', 'Puerto Rico', 'Virgin Islands', 'U.S. Virgin Islands'), 'Region 2',
                            ifelse(i %in% c('District of Columbia', 'Delaware', 'Maryland', 'Pennsylvania', 'Virginia', 'West Virginia'), 'Region 3',
                                   ifelse(i %in% c('Alabama', 'Florida', 'Georgia', 'Kentucky', 'Mississippi', 'North Carolina', 'South Carolina', 'Tennessee'), 'Region 4', 
                                          ifelse(i %in% c('Illinois', 'Indiana', 'Michigan', 'Minnesota', 'Ohio', 'Wisconsin'), 'Region 5',
                                                 ifelse(i %in% c('Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma', 'Texas'), 'Region 6',
                                                        ifelse(i %in% c('Iowa', 'Kansas', 'Missouri', 'Nebraska'), 'Region 7',
                                                               ifelse(i %in% c('Colorado', 'Montana', 'North Dakota', 'South Dakota', 'Utah', 'Wyoming'), 'Region 8',
                                                                      ifelse(i %in% c('Arizona', 'California', 'Hawaii', 'Nevada', 'Pacific Islands', 'American Samoa', 'Guam', 'Baker Island', 'Howland Island', 'Jarvis Island', 'Johnston Atoll', 'Kingman Reef', 'Midway Islands', 'Navassa Island', 'Palmyra Atoll', 'Wake Island', 'Palau','Northern Mariana Islands', 'Micronesia, Federated States of', 'Micronesia', 'Marshall Islands', 'United States Minor Outlying Islands'), 'Region 9',
                                                                             ifelse(i %in% c('Alaska', 'Idaho', 'Oregon', 'Washington'), 'Region 10', NA))))))))))
      result[i] <- OMB
      
      
      
      
    } else {
      
      i <- toupper(i)
      
      OMB <- ifelse(i %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT') ,'Region 1', 
                     ifelse(i %in% c('NJ', 'NY', 'PR', 'VI'), 'Region 2',
                            ifelse(i %in% c('DC', 'DE', 'MD', 'PA', 'VA', 'WV'), 'Region 3',
                                   ifelse(i %in% c('AL', 'FL', 'GA', 'KY', 'MS', 'NC', 'SC', 'TN'), 'Region 4', 
                                          ifelse(i %in% c('IL', 'IN', 'MI', 'MN', 'OH', 'WI'), 'Region 5',
                                                 ifelse(i %in% c('AR', 'LA', 'NM', 'OK', 'TX'), 'Region 6',
                                                        ifelse(i %in% c('IA', 'KS', 'MO', 'NE'), 'Region 7',
                                                               ifelse(i %in% c('CO', 'MT', 'ND', 'sD', 'UT', 'WY'), 'Region 8',
                                                                      ifelse(i %in% c('AZ', 'CA', 'HI', 'NV', 'AS', 'GU', 'MP', 'FM', 'MH', 'UM', 'PW'), 'Region 9',
                                                                             ifelse(i %in% c('AK', 'ID', 'OR', 'WA'), 'Region 10', NA))))))))))
      
      result[i] <- OMB
      
      
    } 
    
  }
  
  result
  
}



