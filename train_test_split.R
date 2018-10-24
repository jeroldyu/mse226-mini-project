# setwd("/Users/jeroldyu/Documents/GitHub/mse226-mini-project")

library(tidyverse)
library(readxl)

set.seed(1)

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

advanced_stats <- read_xlsx("NBA Players - Advanced Season Stats (1978-2016).xlsx",
                             sheet = "Hoja1") %>%
  filter(year < 2016) %>%
  select(-c(column_s, column_x)) %>%
  mutate(truesalary = dollar_to_numeric(truesalary),
         production = dollar_to_numeric(production),
         missing_salary = is.na(truesalary),
         overpaid = truesalary > production
         )

test_idx <- sample(1:nrow(advanced_stats), .2 * nrow(advanced_stats))

train <- advanced_stats[-test_idx,]
test <- advanced_stats[test_idx,]

write.csv(train, "train.csv", row.names = F)
write.csv(test, "test.csv", row.names = F)
