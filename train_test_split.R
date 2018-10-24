# setwd("/Users/jeroldyu/Documents/GitHub/mse226-mini-project")

library(tidyverse)
library(readxl)
source('dollar_to_numeric.R')

set.seed(1)

advanced_stats <- read_xlsx("NBA Players - Advanced Season Stats (1978-2016).xlsx",
                             sheet = "Hoja1") %>%
  filter(year < 2016) %>%
  # removing s and x because they are blank
  # truesalary because its values are incorrect
  # vorp_3 bc it is identical to vorp_2, same with bpm_3 and bpm_2
  # contrib_3 identical to contrib, ovorp_2 and dvorp_2 same as ovorp and dvorp
  # ocontrib_2 and dcontrib_2 same as ocontrib and dcontrib
  select(-c(column_s, column_x, truesalary, 
            vorp_3, bpm_3, contrib_3, ovorp_2, dvorp_2,
            ocontrib_2, dcontrib_2, obpm_2, dbpm_2)) %>%
  mutate(production = dollar_to_numeric(production),
         prod_gm = dollar_to_numeric(prod_gm),
         adjusted_production = dollar_to_numeric(adjusted_production),
         off_playstyle = o_worp > d_worp
         )

test_idx <- sample(1:nrow(advanced_stats), .2 * nrow(advanced_stats))

train <- advanced_stats[-test_idx,]
test <- advanced_stats[test_idx,]

write.csv(train, "train.csv", row.names = F)
write.csv(test, "test.csv", row.names = F)
