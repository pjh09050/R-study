#####################
##  T-square code  ## 
#####################

## 2가지 버전으로작성 
## 차이점은 역행렬과 일반적 영행렬 차이만 존재 나머지 코드는 동일

###############################
##  첫번째 버전 - ginv 사용  ##
###############################



library(MASS)
library(pracma)
data <- read.csv('rul_hrs.csv')
data[data == 0] <- NA
nrow(data)
colSums(is.na(data))
data <- na.omit(data)
nrow(data)
ncol(data)

data <- data[1:128042,3:52]
train_data <- data[1:30000,]
test_data <- data[,]

t_square = function(trdat, tedat,alpha){
  obs = nrow(trdat)
  dim = ncol(trdat)
  mu = colMeans(trdat)
  
  CL = qf(1-alpha, dim, obs -dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  sinv = ginv(cov(trdat))
  mu_mat = repmat(mu, nrow(tedat),1)
  dte = as.matrix(tedat-mu_mat)
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1)
  
  for (i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])))
  }
  
  ret = list(
    Tsq_mat = Tsq_mat,
    CL = CL
  )
  return(ret)
}


################################
##  두번째 버전 - solve 사용  ##
################################
t2 = t_square_solve(train_data, test_data, 0.05)
plot(t2$Tsq_mat, type = 'o', ylim=c(0,15000))
abline(h = c(t2$CL), col = 'red')
abline(v=10500, col = 'blue', lty=2)


t_square_solve = function(trdat, tedat,alpha){
  obs = nrow(trdat)
  dim = ncol(trdat)
  mu = colMeans(trdat)
  
  CL = qf(1-alpha, dim, obs -dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  sinv = solve(cov(trdat))
  mu_mat = repmat(mu, nrow(tedat),1)
  dte = as.matrix(tedat-mu_mat)
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1)
  
  for (i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])))
  }
  
  ret = list(
    Tsq_mat = Tsq_mat,
    CL = CL
  )
  return(ret)
}
