library(pracma)
library(MASS)
## nasa bearin data

df = read.csv('bearing_sample.csv')
head(df)

df = df[,2:5]
head(df)

nrow(df)
tr = as.matrix(df[1:300,])
ts = as.matrix(df[301:nrow(df),])

## main algorithm : mset
mset_regression = function(tr, ts){
  
  y_hat_tr = matrix(0, nrow(tr), ncol(tr))
  
  for (i in 1:ncol(tr)){
    y_hat_tr[,i] = tr[,-i]%*%ginv(t(tr[,-i])%*%tr[,-i])%*%t(tr[,-i])%*%tr[,i]
  }
  
  res = tr - y_hat_tr
  dim(res)
  
  plot(res[,1], type='o')
  abline(h=c(0), col='blue')
  
  # par(mfrow=c(4,1))
  # for(i in 1:4){
  #   plot(res[,i], type='o')
  #   abline(h=c(0), col='blue')
  # }
  
  
  ## testing data
  y_hat_ts = matrix(0, nrow(ts), ncol(ts))
  
  for (i in 1:ncol(ts)){
    y_hat_ts[,i] = ts[,-i]%*%ginv(t(tr[,-i])%*%tr[,-i])%*%t(tr[,-i])%*%tr[,i]
  }
  
  res_ts = ts - y_hat_ts
  
  ret = list(res_tr = res, res_ts = res_ts)
  return(ret)
}

output = mset_regression(tr = as.matrix(iris[1:50, 1:4]), ts = as.matrix(iris[1:150, 1:4]))
plot(output$res_ts[,1], type='o')

output = mset_regression(tr,ts)
plot(output$res_ts[,1], type='o')


## bootstrap control limit
stat = output$res_ts[1:300, 1]
alpha = 0.05
m = 1000

bootlimit = function(stat, alpha, m){
  perc_matrix =matrix(0, 1, m)
  
  for (i in 1:m){
    sample_temp = sample(stat, size=length(stat), replace = TRUE, prob = NULL)
    sample_temp = as.data.frame(sample_temp)
    perc_matrix[,i] <- quantile(sample_temp[,1], 1-alpha)
  }
  return(mean(perc_matrix))
}

abline(h=c(mean(perc_matrix)), col='red')

ucl = bootlimit(output$res_tr, 0.05, 1000)
lcl = bootlimit(output$res_tr, 0.95, 1000)

plot(output$res_ts[,1], type='o')
abline(h = ucl, col='red')
abline(h = lcl, col='red')

alarm = output$res_ts[,1] > ucl | output$res_ts[,1] < lcl
abline(v=c(which(alarm, TRUE)), col='gray', lwd=2)

# which(alarm, TRUE)






