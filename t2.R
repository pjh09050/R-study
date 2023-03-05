#install.packages('pracma')
library(pracma)

iris

train_data = iris[1:50, 1:4]
test_data = iris[1:150, 1:4]

t2 <- function(train_data, test_data, alpha){
  obs = nrow(train_data)
  dim = ncol(train_data)
  mu = colMeans(train_data)
  
  CL = qf(1-alpha, dim,obs-dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  
  cova = cov(train_data)
  icov = solve(cova)
  
  mu_mat = repmat(mu, n = nrow(test_data), 1)
  dte = as.matrix(test_data - mu_mat)
  
  tsq_mat = matrix(0, nrow(test_data), 1)
  
  for(i in 1:nrow(test_data)){
    tsq_mat[i, 1] = t(dte[i,])%*%icov%*%dte[i,]
  }
  ret <- list(
    tsq_mat=tsq_mat,
    CL=CL
  )
  return (ret)
}

tsq = t2(iris[1:50, 1:4], iris[,1:4], alpha=0.1)
tsq$tsq_mat
plot(tsq$tsq_mat, type='o', lwd=2, col = 'blue')
abline(h = c(tsq$CL), col = 'red', lwd=2)

beta = 1-(sum(tsq$tsq_mat[51:150] > tsq$CL)/100)
alp = (sum(tsq$tsq_mat[51:150] > tsq$CL)/100)

