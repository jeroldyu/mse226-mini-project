library(MASS)
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

# LDA/QDA
lda_model = lda(off_playstyle ~ ., data = data_tr)
qda_model = qda(off_playstyle ~ ., data = data_tr)

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

# LDA and QDA
pred_lda = predict(lda_model, data_val)$class
l01_lda = loss01(pred_lda, response)

pred_qda = predict(qda_model, data_val)$class
l01_qda = loss01(pred_qda, response)

#### 4. Fitting models with interactions ####

# possible interactions
# position with blocks
# position with X3par
# position with steals
# position with orb/drb/trb

X_int = model.matrix(off_playstyle ~ . + bbref_pos:blk + bbref_pos:X3par +
                       bbref_pos:stl + bbref_pos:orb + bbref_pos:drb +
                       bbref_pos:trb,
                     data = data_tr)
Y = data_tr$off_playstyle

# Logistic model
logreg_i = glm(off_playstyle ~ . + bbref_pos:blk + bbref_pos:X3par +
                 bbref_pos:stl + bbref_pos:orb + bbref_pos:drb +
                 bbref_pos:trb,
               family = 'binomial', data = data_tr)

# Logistic model with Lasso
logcvLi = cv.glmnet(X_int, Y, family = 'binomial', type.measure = 'auc',
                    alpha = 1, foldid = fold_ids)

# Logistic model with Ridge
lambdas = exp(seq(-10,2,length = 121))
logcvRi = cv.glmnet(X_int, Y, family = 'binomial', type.measure = 'auc',
                    alpha = 0, foldid = fold_ids, lambda = lambdas)

#### 5. Comparing Errors with Interactions ####

# Logistic
predglm_i = predict(logreg_i, newdata = data_val)
l01_log_i = loss01(predglm_i > 0, response)

# Logistic Lasso
X_val_i = model.matrix(off_playstyle ~ . + bbref_pos:blk + bbref_pos:X3par +
                         bbref_pos:stl + bbref_pos:orb + bbref_pos:drb +
                         bbref_pos:trb, data = data_val)

# lambdamin
pred_lmin_i = predict(logcvLi, X_val_i, s = 'lambda.min') %>% as.numeric
l01_lmini = loss01(pred_lmin_i > 0, response)

# lambda1se
pred_l1_i = predict(logcvLi, X_val_i, s = 'lambda.1se') %>% as.numeric
l01_l1i = loss01(pred_l1_i > 0, response)

# Logistic Ridge
# lambdamin
pred_rmin_i = predict(logcvRi, X_val_i, s = 'lambda.min') %>% as.numeric
l01_rmini = loss01(pred_rmin_i > 0, response)

# lambda1se
pred_r1_i = predict(logcvRi, X_val_i, s = 'lambda.1se') %>% as.numeric
l01_r1i = loss01(pred_r1_i > 0, response)

#### 6. Estimating Full Model Test Error ####

X_full_i = model.matrix(off_playstyle ~ . + bbref_pos:blk + bbref_pos:X3par +
                          bbref_pos:stl + bbref_pos:orb + bbref_pos:drb +
                          bbref_pos:trb, data = data)
res_full = data$off_playstyle

set.seed(337)
folds_f = cvFolds(nrow(data), K = 10, R = 1)
fold_ids_f = folds_f$which[folds_f$subsets[,1]]

logcvLi_f = cv.glmnet(X_full_i, res_full, family = 'binomial', type.measure = 'auc',
                    alpha = 1, foldid = fold_ids_f)

pred_lmin_i_f = predict(logcvLi_f, X_full_i, s = 'lambda.min') %>% as.numeric
l01_lmini_f = loss01(pred_lmin_i_f > 0, res_full)
