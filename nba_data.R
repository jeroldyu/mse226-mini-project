# setwd("/Users/jeroldyu/Desktop/ms&e226/project/nba-stats")

library(tidyverse)
library(readxl)

set.seed(1)

advanced_stats <- read_xlsx("NBA Players - Advanced Season Stats (1978-2016).xlsx",
                             sheet = "Hoja1")
test_idx <- sample(1:nrow(advanced_stats), .2 * nrow(advanced_stats))

train <- advanced_stats[-test_idx,]
