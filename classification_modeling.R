library(tidyverse)
library(cvTools)
library(glmnet)
library(pROC)

#### 1. Prepare Data ####

data = read.csv('train.csv')

# covariates:
# age, mp, per, ts, X3par, ftr, orb, drb, trb, ast, stl, blk, tov, usg, (ows, dws / ws), (obpm, dbpm / bpm),
# vorp, shot, team_ts, tm_ortg, tm_drtg, production, prod_gm, as.factor(bbref_pos), yrs_experience, height, weight

data = data %>%
  select(year, age, mp, per, ts, X3par, ftr, orb, drb, trb, ast, stl, blk, tov,
         usg, ws, bpm, vorp, shot, team_ts, tm_ortg, tm_drtg, production,
         prod_gm, bbref_pos, yrs_experience, height, weight, off_playstyle) %>%
  mutate(bbref_pos = as.factor(as.integer(bbref_pos))) %>%
  filter(!is.na(X3par))


set.seed(521)
n = nrow(data)
val_idx = base::sample(n, floor(0.2*n))
data_tr = data[-val_idx,]
data_val = data[val_idx,]


set.seed(226)
folds = cvFolds(nrow(data_tr), K = 10, R = 1)
fold_ids = folds$which[folds$subsets[,1]]

#### 2. Fit Classification Models ####
# we are trying to predict a player's offensive playstyle based on the other features

X = model.matrix(off_playstyle ~ ., data = data_tr)
Y = data_tr$off_playstyle

# Logistic model
logreg = glm(off_playstyle ~ ., family = 'binomial', data = data_tr)

# Logistic model with Lasso
logcvL = cv.glmnet(X, Y, family = 'binomial', type.measure = 'auc',
                   alpha = 1, foldid = fold_ids)

# Logistic model with Ridge
lambdas = exp(seq(-10,2,length = 121))
logcvR = cv.glmnet(X, Y, family = 'binomial', type.measure = 'auc',
                   alpha = 0, foldid = fold_ids, lambda = lambdas)

# others

#### 3. Compare Model Errors ####
# we will use AUC

response = data_val$off_playstyle

# Logistic
predglm = predict(logreg, newdata = data_val)
auc_log = auc(response, predglm) %>% attr('roc') %>% `$`(auc) %>% as.numeric

# Logistic Lasso
X_val = model.matrix(off_playstyle ~ ., data = data_val)

# lambdamin
pred_lmin = predict(logcvL, X_val, s = 'lambda.min') %>% as.numeric
auc_lmin = auc(response, pred_lmin) %>% attr('roc') %>% `$`(auc) %>% as.numeric

# lambda1se
pred_l1 = predict(logcvL, X_val, s = 'lambda.1se') %>% as.numeric
auc_l1 = auc(response, pred_l1) %>% attr('roc') %>% `$`(auc) %>% as.numeric


# Logistic Ridge
# lambdamin
pred_rmin = predict(logcvR, X_val, s = 'lambda.min') %>% as.numeric
auc_rmin = auc(response, pred_rmin) %>% attr('roc') %>% `$`(auc) %>% as.numeric

# lambda1se
pred_r1 = predict(logcvR, X_val, s = 'lambda.1se') %>% as.numeric
auc_r1 = auc(response, pred_r1) %>% attr('roc') %>% `$`(auc) %>% as.numeric


