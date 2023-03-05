# 적분
fx = function(x) 1/((x+1)*sqrt(x))
integrate(fx, lower=0, upper=Inf) # 0 부터 무한대 까지

# 여러 개의 직사각형을 활용하여 적분하기 (직사각형법)
fx = function(x) x^4 - 10*x^3 + 15*x^2 - 6*x + 10
curve(fx, 0, 1)
integr_by_you=function(from,to,n){
  sum=0
  h = (to-from)/n # 전체구간(1-0)을 n개로 나누면 직사각형 밑변의 길이 산출
  for (i in 1:n) sum = sum + h*fx(from+i*h)
  return(sum)
}
integr_by_you(0,1,10)
integrate(fx,0,1)

# 시뮬레이션 활용한 수치 적분
fx = function(x) x^2
integrate(fx, -2,2)
# X = -2~2, Y = 0~4 까지의 영역에서 random으로 데이터 생성
set.seed(132)
a = -2
b = 2
c = 0
d = 4
n = 10^5
x = runif(n,a,b)
y = runif(n,c,d)
temp = sum(y<fx(x))/n
16*temp
