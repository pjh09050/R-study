
library(pracma)

mset_Regress = function(trdat, tedat) {   ## 정상데이터와 이상감지의 대상이 될 데이터에 대한 설정
  
  
  ## training
  y_hat_tr = matrix(numeric(0), nrow(trdat),ncol(trdat) )  ## training 데이터에 대한 matrix화
  
  y_hat_ts = matrix(numeric(0), nrow(tedat),ncol(tedat) ) ## testing 데이터에 대한 matrix화
  
  for ( i in 1:ncol(trdat) ) {
    
    y_hat_tr[,i] = as.matrix(trdat[,-i])%*%ginv(t(as.matrix(trdat[,-i]))%*%as.matrix(trdat[,-i])) %*% t(as.matrix(trdat[,-i]))%*%trdat[,i]
    
    resid_mat_tr = trdat - y_hat_tr;  #### for 문이 각각의 트레이닝셋 변수들에 대해 hat 매트릭스 연산 수행 (remind!! Linear regression)
    
  }
  
  ## testing data 
  for ( i in 1:ncol(tedat) ) {
    
    y_hat_ts[,i] = as.matrix(tedat[,-i])%*%ginv(t(as.matrix(trdat[,-i]))%*%as.matrix(trdat[,-i])) %*% t(as.matrix(trdat[,-i]))%*%trdat[,i]
    resid_mat_ts = tedat - y_hat_ts; #### for 문이 각각의 테스팅셋 변수들에 대해 hat 매트릭스 연산 수행 
  }
  
  ret <- list(
    
    resid_tr = resid_mat_tr, 
    
    resid_ts = resid_mat_ts
    
  )
  ## testing
  return(ret)
}
