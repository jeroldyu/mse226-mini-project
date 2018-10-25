library(tidyverse)

train = read.csv('train.csv') 

train_trimmed = train %>%
  select(-tm, -player, -player_id)

offensive_cors = cor(select(train_trimmed, off_playstyle),
                     select(train_trimmed, -off_playstyle),
                     use = "na.or.complete") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(off_playstyle)) %>%
  column_to_rownames()
