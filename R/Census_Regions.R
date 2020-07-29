#' This function will take either a vector of U.S. state/territories names or abbreviations and generate a new column identifying which of the 10 Census Regions the state/territory belongs to.
#' The function will automatically make state/territory names title case and make abbreviations uppercase.
#' It attempts to capture all territories, including the various Pacific Islands, by identifying a range of ways they might be named.


census_region <- function(x) {

  result <- vector('character')

  for (i in x) {

    if (nchar(as.character(i)) > 2) {

      i <- str_to_title(i)


      Census <- ifelse(i %in% c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont', 'New Jersey', 'New York', 'Pennsylvania'), 'Region 1',
                       ifelse(i %in% c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin', 'Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'South Dakota'), 'Region 2',
                              ifelse(i %in% c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 'District of Columbia', 'Washington D.C.', 'Washington DC', 'West Virginia', 'Alabama', 'Kentucky', 'Mississippi', 'Tennessee', 'Arkansas', 'Louisiana', 'Oklahoma', 'Texas'), 'Region 3',
                                     ifelse(i %in% c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Utah', 'Wyoming', 'Alaska', 'California', 'Hawaii', 'Oregon', 'Washington'), 'Region 4', NA))))

      result[i]  <- Census



    } else {

      i <- toupper(i)

      Census <- ifelse(i %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 'NJ', 'NY', 'PA'), 'Region 1',
                       ifelse(i %in% c('IL', 'IN', 'MI', 'OH', 'WI', 'IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD'), 'Region 2',
                              ifelse(i %in% c('DE', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'DC', 'WV', 'AL', 'KY', 'MS', 'TN', 'AR', 'LA', 'OK', 'TX'), 'Region 3',
                                     ifelse(i %in% c('AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 'CA', 'HI', 'OR', 'WA'), 'Region 4', NA))))

      result[i] <- Census

    }

  }
  result
}



