# 3d plot
x <- seq(-5,5,length=100)
y <- x
f <- function(x,y) (x-1)^2+(2*y-1)^2
z <- outer(x,y,f) # 외적
op <- par(bg='white')
persp(x,y,z,theta=60,phi=30,col='lightblue')


## bivariate
g_bi = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) # initial gradient만큼 이동
  b = x-oldx 
  vecnorm = sqrt(sum(b^2)) # 벡터놈 정의
  
  while ((vecnorm)>tol){ # 다변수에서는 이동량을 벡터놈으로 정의
    iter = iter +1
    if (iter > m) return(x) # 수렴횟수보다 넘어서면 결과값 산출
    oldx = x
    x = x -h*fp(x) # 부호가 +라면 gradient ascent
  }
  return (x)
}
fp = function(x){  ##  다변수미분을 위한 방정식 셋팅 (전장의 식을 미분한 다변수 도함수)
  x1 = 2*x[1]-2    ## x[1] : x벡터중 첫번째 element
  x2 = 8*x[2]-4    ## x[2] : x벡터중 두번째 element
  return(c(x1,x2))
}
g_bi(fp, c(0,0),m=1000) 
g_bi(fp, c(100,100), m=1000)
fp(c(1,0.5))