#install.packages("glmnet")
#install.packages('dplyr')
library(glmnet)
library('dplyr')

df = read.csv('rul_hrs.csv')
df = df[3:53]

x = as.matrix(df[,1:50])
y = df[,51]

# ridge
grid = 10^seq(10,-2, length=100)
ridge.mod = glmnet(x,y,alpha=0, lambda=grid)
summary(ridge.mod)

ridge_cv <- cv.glmnet(x, y, alpha=0, lambda = lambdas)
best_lambda <- ridge_cv$lambda.min
best_lambda # 0.001

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge <- glmnet(x,y,alpha=0,lambda=0.01)
coef(best_ridge)
ridge_coef = predict(best_ridge, type='coefficients', s=best_lambda)[1:51,]
ridge_coef
length(ridge_coef[ridge_coef != 0])

# lasso
grid = 10^seq(10,-2, length=100)
lasso_mod = glmnet(x,y,alpha=1, lambda=grid)
plot(lasso_mod)

lasso_cv <- cv.glmnet(x, y, alpha=0)
bestlam <- lasso_cv$lambda.min
lasso_pred = predict(lasso_mod, s=bestlam, newx=x)
best_lasso = glmnet(x, y, alpha=1, lambda=best_lambda)
coef(best_lasso)


# ridge.mod$lambda[50]
# coef(ridge.mod)[,50]
# # calculate l2 norm
# sqrt(sum(coef(ridge.mod)[-1,50]^2)) # 0.958
# 
# ridge.mod$lambda[60]
# coef(ridge.mod)[,60]
# # calculate l2 norm
# sqrt(sum(coef(ridge.mod)[-1,60]^2)) # 13.79
# a = predict(ridge.mod,s=50,type="coefficients")[1:20,]
# 
# ridge.mod 
# 
# # 적절한 lambda 찾기
# set.seed(1)
# cv.out=cv.glmnet(x,y,alpha=0)
# plot(cv.out)
# 
# bestlam=cv.out$lambda.min
# bestlam
# 
# ridge.pred=predict(ridge.mod,s=bestlam,newx=x)
# mean((ridge.pred-y)^2)
# 
# out=glmnet(x,y,alpha=0)
# pred = predict(out,type="coefficients",s=bestlam)[1:51,]
# length(pred[pred!=0])

# lasso
grid = 10^seq(10,-2,length=100)
lasso.mod = glmnet(x,y,alpha=1, lambda=grid)
#plot(lasso.mod)

cv.out1=cv.glmnet(x,y,alpha=0)
#plot(cv.out1)
#
# # 적절한 lambda
bestlam1=cv.out1$lambda.min
bestlam1

best_model <- glmnet(x, y, alpha=1, lambda=bestlam1)
coef(best_model)

lasso.pred=predict(lasso.mod,s=bestlam1,newx=x)
mean((lasso.pred-y)^2)

out=glmnet(x,y,alpha=1, lambda=bestlam1)
lasso.coef = predict(out,type="coefficients",s=bestlam1)[1:51,]
length(lasso.coef[lasso.coef!=0])

bestlam1=cv.out1$lambda.1se
lasso.pred1=predict(lasso.mod,s=bestlam1,newx=x)
mean((lasso.pred1-y)^2)

lasso.coef1=predict(out,type="coefficients",s=bestlam1)[1:51,]
lasso.coef1 #cof값을 확인해보니 25개가 살아남음(단순한 모델을 원할 땐 위와 같이 사용하면됨)
a = lasso.coef1[lasso.coef1!=0]
names(a)
length(a)

