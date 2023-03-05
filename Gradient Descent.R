# 미분 방법 1
f = readline(prompt = "function?")
f
f = parse(text=f)
class(f)
fd = D(f, "x")
fd
f = function(x) 0.25*x^4+x^3-x-1
# 미분값
fp = 0.25 * (4 * x^3) + 3 * x^2 - 1

g_desc = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) # initial gradient만큼 이동
  while (abs(x-oldx)>tol){
    iter = iter +1
    if (iter > m) stop("max iteration")
    oldx = x
    x = x -h*fp(x) # 부호가 +라면 gradient ascent
  }
  return (x)
}

fd = function(x) 0.25 * (4 * x^3) + 3 * x^2 - 1
fd

res = g_desc(fd,-1)
res
res = g_desc(fd,-10)
res
res = g_desc(fd,2)
res

f = function(x) 0.25*x^4+x^3-x-1
curve(f,-4,2)

abline(v=c(res), lty=2, col='blue')
res
res = g_desc(fd,2)
res
abline(v=c(res), lty=1, col='red')
