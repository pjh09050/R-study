#install.packages('xgboost')
#install.packages('caret')
library(caret)
library(rpart.plot)
library(tidyverse)
library(xgboost)
library(MASS)

data<- read.csv('rul_hrs.csv')
data <- data[,3:52]
rul <- data[,53]
df <- cbind(data, rul)
df <- as.data.frame(df)

train = df[1:128041,] # Create the training data 
test = df[128042:166441,] # Create the test data

train_x = data.matrix(train[, -51])
train_y = train[,51]

test_x = data.matrix(test[, -51])
test_y = test[, 51]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)
watchlist = list(train=xgb_train, test=xgb_test)

train_control = trainControl(method = "cv", number = 5, search = "grid")
set.seed(50)
gbmGrid <-  expand.grid(max_depth = (5:15),
                        nrounds = (1:10)*50,
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)
#model = train(rul~., data = train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbose=FALSE, verbosity=0)
#model = xgb.train(data = xgb_train, max.depth = 15, watchlist=watchlist, nrounds = 500, eta=0.3, colsample_bytree = 0.6)
model_xgboost = xgboost(data = xgb_train, max.depth = 12, nrounds = 100, verbose = 0, eta=0.3, colsample_bytree=0.6)

pred_y = predict(model_xgboost, xgb_test)

ks.test(pred_y, test_y)
mean((test_y - pred_y)^2) #mse - Mean Squared Error
caret::RMSE(test_y, pred_y) #rmse - Root Mean Squared Error

x = 1:length(test_y) 
plot(x, test_y, col = "red", type = "l") 
lines(x, pred_y, col = "blue", type = "l") 
legend(x = 100, y = 38, legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1) )

MAPE = function(test_y,pred_y){
  mean(abs((test_y-pred_y)/test_y))*100
}
RSQUARE = function(test_y,pred_y){
  cor(test_y,pred_y)^2
}
LR_MAPE = MAPE(test_y,pred_y) # Using MAPE error metrics to check for the error rate and accuracy level
LR_R = RSQUARE(test_y,pred_y) # Using R-SQUARE error metrics to check for the error rate and accuracy level
Accuracy_Linear = 100 - LR_MAPE

# feature importance
importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgboost)
importance_matrix
xgb.plot.importance(importance_matrix[1:47,])



