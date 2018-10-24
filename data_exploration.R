setwd("/Users/jeroldyu/Documents/GitHub/mse226-mini-project")

library(tidyverse)

train <- read.csv("train.csv", as.is = T)

for (col in colnames(train)) {
  print(paste0("This is column ", col, ": ", length(which(is.na(train[col])))))
}


# Get mean salaries for each year into a player's career; will be used for
# potential imputation on player's who don't have filled in salary entries.
mean_salaries <- train %>%
  filter(!is.na(truesalary)) %>%
  mutate(salary = dollar_to_numeric(truesalary)) %>%
  group_by(yrs_experience) %>%
  summarise(mean_salary = mean(salary))

ggplot(mean_salaries) +
  geom_point(aes(x = yrs_experience, y = mean_salary)) +
  geom_line(aes(x = yrs_experience, y = mean_salary))


# TODO: find the covariates that are the most/least correlated with the continuous
# response variable team_mar.

ggplot(train) +
  geom_boxplot(mapping = aes(x = -age, y = team_mar, group = age)) +
  coord_flip()

train_numeric <- train %>% 
  mutate(truesalary = dollar_to_numeric(truesalary)) %>%
  select_if(is.numeric)

train_numeric_subset <- cbind(train_numeric[,1:12], train_numeric %>% select(team_mar))


cor_matrix <- cor(train %>% 
                    select(team_mar), 
                  train_numeric %>%
                    select(-team_mar),
                  use = "na.or.complete") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(team_mar)) %>%
  column_to_rownames()

train_numeric_subset %>%
  gather(-team_mar, key = "some_var_name", value = "some_value_name") %>%
  ggplot(aes(x = some_value_name, y = team_mar)) +
  geom_point() +
  facet_wrap(~ some_var_name, scales = "free")

with(train, cor(tm_ortg, tm_drtg))

pairs(train %>%
        select(tm_ortg, tm_drtg, team_ts, dws_48, ws_48))
cor(train %>%
        select(tm_ortg, tm_drtg, team_ts, dws_48, ws_48))
cor(train %>%
      select(tm_ortg, tm_drtg, team_ts, mpg, usg))
