#' This function will take either a vector of U.S. state/territories names or abbreviations and generate a new column identifying which of the 10 Census Regions the state/territory belongs to.
#' The function will automatically make state/territory names title case and make abbreviations uppercase.
#' US territories are not part of any Census region/division.


census_division <- function(x) {

  result <- vector('character')

  for (i in x) {

    if (nchar(as.character(i)) > 2) {

      i <- str_to_title(i)


      Census <- ifelse(i %in% c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont'), 'Division 1', 
                       ifelse(i %in% c('New Jersey', 'New York', 'Pennsylvania'), 'Division 2',
                              ifelse(i %in% c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin'), 'Division 3',
                                     ifelse(i %in% c('Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'South Dakota'), 'Division 4',
                                            ifelse(i %in% c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 'District of Columbia', 'Washington D.C.', 'Washington DC', 'West Virginia'), 'Division 5',
                                                   ifelse(i %in% c('Alabama', 'Kentucky', 'Mississippi', 'Tennessee'), 'Division 6',
                                                          ifelse(i %in% c('Arkansas', 'Louisiana', 'Oklahoma', 'Texas'), 'Division 7',
                                                                 ifelse(i %in% c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Utah', 'Wyoming'), 'Division 8',
                                                                        ifelse(i %in% c('Alaska', 'California', 'Hawaii', 'Oregon', 'Washington'), 'Division 9', NA)))))))))

      result[i]  <- Census



    } else {

      i <- toupper(i)

      Census <- ifelse(i %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT'), 'Division 1', 
                       ifelse(i %in% c('NJ', 'NY', 'PA'), 'Division 2',
                              ifelse(i %in% c('IL', 'IN', 'MI', 'OH', 'WI'), 'Division 3',
                                     ifelse(i %in% c('IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD'), 'Division 4',
                                            ifelse(i %in% c('DE', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'DC', 'WV'), 'Division 5',
                                                   ifelse(i %in% c('AL', 'KY', 'MS', 'TN'), 'Division 6',
                                                          ifelse(i %in% c('AR', 'LA', 'OK', 'TX'), 'Division 7',
                                                                 ifelse(i %in% c('AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY'), 'Division 8',
                                                                        ifelse(i %in% c('AK', 'CA', 'HI', 'OR', 'WA'), 'Division 9', NA)))))))))
        
      result[i] <- Census

    }

  }
  result
}



