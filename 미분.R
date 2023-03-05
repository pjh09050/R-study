# 미분
## fd
fd = function(f,x,h = x*sqrt(.Machine$double.eps)){
  return((f(x+h)-f(x))/h)
}
f = function(x) - x^2 + 6*x - 6
curve(f,1,5)
fd(f,2,h = 0.1)
fd(f,2,h = 1)
fd(f,2,h = .5)
fd(f,2,h = .1)
fd(f,2,h = .1e-6) # h 가 작을수록 유한 차분값은 정확한 값에 근사함

## real mi bun
## 중요 !
dfx = expression(-x^2 + 6*x - 6, "x") # expression : 수식표현
D(dfx,'x') # x 에 대해 미분 
f <- function(x) eval(c(dfx)[[1]])
f(2)

##
fxyz = expression((x*y)^5 - 1/x^z + cos(x)^x)
fxyz
dfx = D(fxyz, "x")
print(dfx)
exp = deriv(fxyz, c('x', 'y','z'))
exp
f2 = function(x,y,z) eval(c(exp))
f2(1,1,1)