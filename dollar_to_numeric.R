# Converts a dollar amount represented as a string (e.g. $1,000.25) into a 
# numeric value (e.g. 1000.25).
dollar_to_numeric <- function(dollar) {
  dollar <- gsub(",", "", dollar)
  dollar <- gsub("(", "", dollar, fixed=TRUE)
  dollar <- gsub(")", "", dollar, fixed=TRUE)
  dollar %>%
    as.character() %>%
    substring(2) %>%
    as.double()
}