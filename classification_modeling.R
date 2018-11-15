library(tidyverse)
library(cvTools)
library(glmnet)
library(class)

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

# K-Nearest Neighbors
dtX = select(data_tr, -off_playstyle)
dvX = select(data_val, -off_playstyle)

set.seed(110)
ks = c(1, 5, 10, 20, 50, 100, 200)
knns = lapply(ks, function(x) {
  knn(dtX, dvX, data_tr$off_playstyle, k = x)
})

#### 3. Compare Model Errors ####
# we will use 0-1 loss

response = data_val$off_playstyle

loss01 = function(preds, response) {
  mean(preds != response)
}


# Logistic
predglm = predict(logreg, newdata = data_val)
l01_log = loss01(predglm > 0, response)

# Logistic Lasso
X_val = model.matrix(off_playstyle ~ ., data = data_val)

# lambdamin
pred_lmin = predict(logcvL, X_val, s = 'lambda.min') %>% as.numeric
l01_lmin = loss01(pred_lmin > 0, response)

# lambda1se
pred_l1 = predict(logcvL, X_val, s = 'lambda.1se') %>% as.numeric
l01_l1 = loss01(pred_l1 > 0, response)

# Logistic Ridge
# lambdamin
pred_rmin = predict(logcvR, X_val, s = 'lambda.min') %>% as.numeric
l01_rmin = loss01(pred_rmin > 0, response)

# lambda1se
pred_r1 = predict(logcvR, X_val, s = 'lambda.1se') %>% as.numeric
l01_r1 = loss01(pred_r1 > 0, response)

#knns
l01_knn = vapply(knns, function(x) {
  loss01(x, response)
}, 0.0)
names(l01_knn) = ks
# best KNN model: k = 50, still a val.set misclassification rate of 0.341 (terrible!)

