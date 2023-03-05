
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
#install.packages('repr')
library(repr)

dat = read.csv('rul_hrs.csv')
dat = dat[,3:53]
glimpse(dat)
which(dat$rul==0)

# 데이터 파티셔닝
train = dat[1:141132,] # Create the training data 
test = dat[141133:166441,] # Create the test data
dim(train)
dim(test)

# 숫자 기능 스케일링
pre_proc_val <- preProcess(train[,1:50], method = c("center", "scale"))
train[,1:50] = predict(pre_proc_val, train[,1:50])
test[,1:50] = predict(pre_proc_val, test[,1:50])
summary(train)

# 선형 회귀
lr = lm(rul ~  sensor_00 + sensor_01 + sensor_02 + sensor_03 + sensor_04 + sensor_05 + 
          sensor_06 + sensor_07 + sensor_08 + sensor_09 + sensor_10 + sensor_11 + 
          sensor_12 + sensor_13 + sensor_14 + sensor_16 + sensor_17 + sensor_18 + 
          sensor_19 + sensor_20 + sensor_21 + sensor_22 + sensor_23 + sensor_24 + 
          sensor_25 + sensor_26 + sensor_27 + sensor_28 + sensor_29 + sensor_30 + 
          sensor_31 + sensor_32 + sensor_33 + sensor_34 + sensor_35 + sensor_36 + 
          sensor_37 + sensor_38 + sensor_39 + sensor_40 + sensor_41 + sensor_42 + 
          sensor_43 + sensor_44 + sensor_45 + sensor_46 + sensor_47 + sensor_48 + 
          sensor_49 + sensor_51, data = train)
lr1 = lm(rul ~  sensor_00 + sensor_01 + sensor_02 + sensor_03 + sensor_04 + sensor_05 + 
          sensor_07 + sensor_08 + sensor_09 + sensor_10 + sensor_11 + 
          sensor_12 + sensor_13 + sensor_14 + sensor_16 + sensor_17 + sensor_18 + 
          sensor_19 + sensor_20 + sensor_21 + sensor_22 + sensor_23 + sensor_24 + 
          sensor_25 + sensor_27 + sensor_28 + sensor_29 + sensor_30 + 
          sensor_31 + sensor_32 + sensor_33 + sensor_34 + sensor_35 + sensor_36 + 
          sensor_37 + sensor_38 + sensor_39 + sensor_40 + sensor_41 + sensor_42 + 
          sensor_43 + sensor_44 + sensor_45 + sensor_46 + sensor_47 + sensor_48 + 
          sensor_51, data = train)
summary(lr1)

# 모델 평가 지표
#Step 1 - create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}
# Step 2 - predicting and evaluating the model on train data
predictions = predict(lr1, newdata = train)
eval_metrics(lr1, train, predictions, target = 'rul')
# Step 3 - predicting and evaluating the model on test data
predictions = predict(lr1, newdata = test)
eval_metrics(lr1, test, predictions, target = 'rul')


# 정규화
dummies <- dummyVars(rul ~ ., data = dat[,1:51])
train_dummies = predict(dummies, newdata = train[,1:51])
test_dummies = predict(dummies, newdata = test[,1:51])
print(dim(train_dummies))
print(dim(test_dummies))

# ridge regression
library(glmnet)
x = as.matrix(train_dummies)
y_train = train$rul

x_test = as.matrix(test_dummies)
y_test = test$rul
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_lambda)

summary(ridge_reg)

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)


# lasso regression
lambdas <- 10^seq(2, -3, by = -.1)
# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)

