
data = datasets::mtcars
data
x = matrix(data$mpg, ncol=1)
y = matrix(data$vs, ncol=1)
w = runif(n=1)
b = runif(n=1)
plot(x, y)

sigmoid = function(x) {
  return(1/(1+exp(-x)))
}
lines(x, predict(x), type="l", col="red")

loss_func = function(x, t){
  delta = exp(1)-2
  z = x%*%w + b
  y = sigmoid(z)
  return( -sum (t*log(y+delta) + (1-t)*log((1-y)+delta)))
}
loss_func(x, 0.01)

g_desc = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) 
  while (abs(x-oldx)>tol){
    iter = iter +1
    if (iter > m) stop("max iteration")
    oldx = 1
    x = x -h*fp(X)
  }
  return (x)
}
res = g_desc(fp, 1)

logistic_regression = function(x, learning_rate = 0.1, epochs=1000){
  w = 0
  b = 0
  for (i in 1:epochs+1){
    w_difference = x*(sigmoid(w*x+b)-y)
    b_difference = sigmoid(w*x+b)-y
    w = w - (learning_rate*w_difference)
    b = b - (learning_rate*w_difference)
    return (w, b)
  }
}

predict = function(x){
  z1 = (x%*%w) + b
  y = sigmoid(z1)
  if(y>0.5){
    result = 1
  }
  else {
    result = 0
  }
  return(result)
}
predict(x)

