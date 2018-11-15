##### Libraries #####
library(tidyverse)
library(glmnet)
library(cvTools)
library(quantreg)
library(rqPen)


##### Load data #####
data <- read.csv("train.csv", as.is = T) %>%
  filter(!is.na(ftr) & !is.na(X3par)) %>%
  mutate(bbref_pos = as.factor(floor(bbref_pos)))

# Obtain two different set of covariates: ones which separate offense and defense, and
# ones that combine them into one metric. 1 corresponds to features with offense/defense
# and 2 corresponds to combined metric.
desired_cols_1 <- c("g", "mp", "per", "ftr", "orb", "drb", "ast", "stl", "blk",
                    "tov", "usg", "ows", "dws", "obpm", "dbpm", "ows_48", "X3par",
                    "dws_48", "team_mar", "tm_ts_w_o_plyr", "ovorp", "dvorp",
                    "o_worp", "d_worp", "yrs_experience", "bbref_pos")
df1 <- data[,desired_cols_1]

desired_cols_2 <- c("g", "mp", "per", "ftr", "trb", "ast", "stl", "blk",
                    "tov", "usg", "ws", "ws_48", "bpm", "team_mar", "X3par",
                    "tm_ts_w_o_plyr", "vorp", "worp", "yrs_experience", "bbref_pos")
df2 <- data[,desired_cols_2]

X1 <- model.matrix(team_mar ~ ., data = df1)[,-1]
Y1 <- df1$team_mar

X2 <- model.matrix(team_mar ~ ., data = df2)[,-1]
Y2 <- df2$team_mar

X3 <- model.matrix(team_mar ~ . + bbref_pos:orb + bbref_pos:drb + bbref_pos:blk, data = df1)[,-1]
X4 <- model.matrix(team_mar ~ . + bbref_pos:trb + bbref_pos:blk, data = df2)[,-1]

##### Parameter Tuning #####
# Separate into training/validation set to tune lambda for ridge/lasso, through 
# 10-fold CV on the training set. For each model, we will evaluate how well the 
# optimal parameter and the parameter chosen according to the one-standard-error
# rule perform on the validation set. The parameter that performs the best on the
# validation set will be used in the model selection step.
set.seed(521)

val.idx <- sample(nrow(data), floor(0.2 * nrow(data)))

# Create folds for CV on training data.
set.seed(226)

folds <- cvFolds(nrow(train), K = 10, R = 1)
fold_ids <- folds$which[folds$subsets[,1]]

# RIDGE parameter tuning
# 1st model: offense/defense features
ridge.cv1 <- cv.glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 0, foldid = fold_ids)

ridge.fm1.min <- glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 0)
ridge.fm1.min.pred <- predict(ridge.fm1.min, s = ridge.cv1$lambda.min, newx = X1[val.idx,])
ridge.fm1.min.mse <- mean((ridge.fm1.min.pred - Y1[val.idx])^2)

ridge.fm1.1se <- glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 0)
ridge.fm1.1se.pred <- predict(ridge.fm1.1se, s = ridge.cv1$lambda.1se, newx = X1[val.idx,])
ridge.fm1.1se.mse <- mean((ridge.fm1.1se.pred - Y1[val.idx])^2)

c(ridge.cv1$lambda.1se, ridge.cv1$lambda.min)
ridge1_param <- ifelse(ridge.fm1.min.mse < ridge.fm1.1se.mse, ridge.cv1$lambda.min, ridge.cv1$lambda.1se)

# 2nd model: combined features
ridge.cv2 <- cv.glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 0, foldid = fold_ids)

ridge.fm2.min <- glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 0)
ridge.fm2.min.pred <- predict(ridge.fm2.min, s = ridge.cv2$lambda.min, newx = X2[val.idx,])
ridge.fm2.min.mse <- mean((ridge.fm2.min.pred - Y2[val.idx])^2)

ridge.fm2.1se <- glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 0)
ridge.fm2.1se.pred <- predict(ridge.fm2.1se, s = ridge.cv2$lambda.1se, newx = X2[val.idx,])
ridge.fm2.1se.mse <- mean((ridge.fm2.1se.pred - Y2[val.idx])^2)

c(ridge.cv2$lambda.1se, ridge.cv2$lambda.min)
ridge2_param <- ifelse(ridge.fm2.min.mse < ridge.fm2.1se.mse, ridge.cv2$lambda.min, ridge.cv2$lambda.1se)

# 3rd model: 1st model with interaction terms
ridge.cv3 <- cv.glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 0, foldid = fold_ids)

ridge.fm3.min <- glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 0)
ridge.fm3.min.pred <- predict(ridge.fm3.min, s = ridge.cv3$lambda.min, newx = X3[val.idx,])
ridge.fm3.min.mse <- mean((ridge.fm3.min.pred - Y1[val.idx])^2)

ridge.fm3.1se <- glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 0)
ridge.fm3.1se.pred <- predict(ridge.fm3.1se, s = ridge.cv3$lambda.1se, newx = X3[val.idx,])
ridge.fm3.1se.mse <- mean((ridge.fm3.1se.pred - Y1[val.idx])^2)

c(ridge.cv3$lambda.1se, ridge.cv3$lambda.min)
ridge3_param <- ifelse(ridge.fm3.min.mse < ridge.fm3.1se.mse, ridge.cv3$lambda.min, ridge.cv3$lambda.1se)

# 4th model: combined features
ridge.cv4 <- cv.glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 0, foldid = fold_ids)

ridge.fm4.min <- glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 0)
ridge.fm4.min.pred <- predict(ridge.fm4.min, s = ridge.cv4$lambda.min, newx = X4[val.idx,])
ridge.fm4.min.mse <- mean((ridge.fm4.min.pred - Y2[val.idx])^2)

ridge.fm4.1se <- glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 0)
ridge.fm4.1se.pred <- predict(ridge.fm4.1se, s = ridge.cv4$lambda.1se, newx = X4[val.idx,])
ridge.fm4.1se.mse <- mean((ridge.fm4.1se.pred - Y2[val.idx])^2)

c(ridge.cv4$lambda.1se, ridge.cv4$lambda.min)
ridge4_param <- ifelse(ridge.fm4.min.mse < ridge.fm4.1se.mse, ridge.cv4$lambda.min, ridge.cv4$lambda.1se)


# LASSO parameter tuning
# 1st model: offense/defense features
lasso.cv1 <- cv.glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 1, foldid = fold_ids)

lasso.fm1.min <- glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 1)
lasso.fm1.min.pred <- predict(lasso.fm1.min, s = lasso.cv1$lambda.min, newx = X1[val.idx,])
lasso.fm1.min.mse <- mean((lasso.fm1.min.pred - Y1[val.idx])^2)

lasso.fm1.1se <- glmnet(X1[-val.idx,], Y1[-val.idx], alpha = 1)
lasso.fm1.1se.pred <- predict(lasso.fm1.1se, s = lasso.cv1$lambda.1se, newx = X1[val.idx,])
lasso.fm1.1se.mse <- mean((lasso.fm1.1se.pred - Y1[val.idx])^2)

c(lasso.cv1$lambda.min, lasso.cv1$lambda.1se)
lasso1_param <- ifelse(lasso.fm1.min.mse < lasso.fm1.1se.mse, lasso.cv1$lambda.min, lasso.cv1$lambda.1se)

# 2nd model: combined features
lasso.cv2 <- cv.glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 1, foldid = fold_ids)

lasso.fm2.min <- glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 1)
lasso.fm2.min.pred <- predict(lasso.fm2.min, s = lasso.cv2$lambda.min, newx = X2[val.idx,])
lasso.fm2.min.mse <- mean((lasso.fm2.min.pred - Y2[val.idx])^2)

lasso.fm2.1se <- glmnet(X2[-val.idx,], Y2[-val.idx], alpha = 1)
lasso.fm2.1se.pred <- predict(lasso.fm2.1se, s = lasso.cv2$lambda.1se, newx = X2[val.idx,])
lasso.fm2.1se.mse <- mean((lasso.fm2.1se.pred - Y2[val.idx])^2)

c(lasso.cv2$lambda.min, lasso.cv2$lambda.1se)
lasso2_param <- ifelse(lasso.fm2.min.mse < lasso.fm2.1se.mse, lasso.cv2$lambda.min, lasso.cv2$lambda.1se)

# 3rd model: 1st model with interaction terms
lasso.cv3 <- cv.glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 1, foldid = fold_ids)

lasso.fm3.min <- glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 1)
lasso.fm3.min.pred <- predict(lasso.fm3.min, s = lasso.cv3$lambda.min, newx = X3[val.idx,])
lasso.fm3.min.mse <- mean((lasso.fm3.min.pred - Y1[val.idx])^2)

lasso.fm3.1se <- glmnet(X3[-val.idx,], Y1[-val.idx], alpha = 1)
lasso.fm3.1se.pred <- predict(lasso.fm3.1se, s = lasso.cv3$lambda.1se, newx = X3[val.idx,])
lasso.fm3.1se.mse <- mean((lasso.fm3.1se.pred - Y1[val.idx])^2)

c(lasso.cv3$lambda.min, lasso.cv3$lambda.1se)
lasso3_param <- ifelse(lasso.fm3.min.mse < lasso.fm3.1se.mse, lasso.cv3$lambda.min, lasso.cv3$lambda.1se)

# 4th model: combined features
lasso.cv4 <- cv.glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 1, foldid = fold_ids)

lasso.fm4.min <- glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 1)
lasso.fm4.min.pred <- predict(lasso.fm4.min, s = lasso.cv4$lambda.min, newx = X4[val.idx,])
lasso.fm4.min.mse <- mean((lasso.fm4.min.pred - Y2[val.idx])^2)

lasso.fm4.1se <- glmnet(X4[-val.idx,], Y2[-val.idx], alpha = 1)
lasso.fm4.1se.pred <- predict(lasso.fm4.1se, s = lasso.cv4$lambda.1se, newx = X4[val.idx,])
lasso.fm4.1se.mse <- mean((lasso.fm4.1se.pred - Y2[val.idx])^2)

c(lasso.cv4$lambda.min, lasso.cv4$lambda.1se)
lasso4_param <- ifelse(lasso.fm4.min.mse < lasso.fm4.1se.mse, lasso.cv4$lambda.min, lasso.cv4$lambda.1se)



##### Model selection #####
# Using the optimal parameters selected in the parameter tuning section, the following
# models will be evaluated on the entire training data:
# - Linear regression
# - Ridge regression
# - The Lasso
# The model that gives us the lowest CV error will be the mdoel chosen.

set.seed(221)

folds <- cvFolds(nrow(data), K = 10, R = 1)
fold_ids <- folds$which[folds$subsets[,1]]

# RIDGE
ridge.fm1 <- cv.glmnet(X1, Y1, alpha = 0, foldid = fold_ids)
ridge.mse1 <- mean((predict(ridge.fm1, X1, s=ridge1_param) - Y1)^2)

ridge.fm2 <- cv.glmnet(X2, Y2, alpha = 0, foldid = fold_ids)
ridge.mse2 <- mean((predict(ridge.fm2, X2, s=ridge2_param) - Y2)^2)

ridge.fm3 <- cv.glmnet(X3, Y1, alpha = 0, foldid = fold_ids)
ridge.mse3 <- mean((predict(ridge.fm3, X3, s=ridge3_param) - Y1)^2)

ridge.fm4 <- cv.glmnet(X4, Y2, alpha = 0, foldid = fold_ids)
ridge.mse4 <- mean((predict(ridge.fm4, X4, s=ridge4_param) - Y2)^2)

# LASSO
lasso.fm1 <- cv.glmnet(X1, Y1, alpha = 0, foldid = fold_ids)
lasso.mse1 <- mean((predict(lasso.fm1, X1, s=lasso1_param) - Y1)^2)

lasso.fm2 <- cv.glmnet(X2, Y2, alpha = 0, foldid = fold_ids)
lasso.mse2 <- mean((predict(lasso.fm2, X2, s=lasso2_param) - Y2)^2)

lasso.fm3 <- cv.glmnet(X3, Y1, alpha = 0, foldid = fold_ids)
lasso.mse3 <- mean((predict(lasso.fm3, X3, s=lasso3_param) - Y1)^2)

lasso.fm4 <- cv.glmnet(X4, Y2, alpha = 0, foldid = fold_ids)
lasso.mse4 <- mean((predict(lasso.fm4, X4, s=lasso4_param) - Y2)^2)

# LR
lr.mod1 <- lm(Y1 ~ ., data = data.frame(X1, Y1))
lr.fm1 <- cvLm(lr.mod1, folds = folds)
lr.mse1 <- lr.fm1$cv^2

lr.mod2 <- lm(Y2 ~ ., data = data.frame(X2, Y2))
lr.fm2 <- cvLm(lr.mod2, folds = folds)
lr.mse2 <- lr.fm2$cv^2

lr.mod3 <- lm(Y1 ~ ., data = data.frame(X3, Y1))
lr.fm3 <- cvLm(lr.mod3, folds = folds)
lr.mse3 <- lr.fm3$cv^2

lr.mod4 <- lm(Y2 ~ ., data = data.frame(X4, Y2))
lr.fm4 <- cvLm(lr.mod4, folds = folds)
lr.mse4 <- lr.fm4$cv^2

# Quantile regression with p = 0.5
qr.err <- rep(NA, 10)
for (k in 1:10) {
  train <- which(fold_ids != k)
  qr.mod <- rq(team_mar ~ ., data = df1, tau = 0.5, subset = train)
  qr.err[k] <- mean((df1$team_mar[-train] - predict(qr.mod, df1[-train,]))^2)
}
mean(qr.err)