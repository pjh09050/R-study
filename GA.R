

rastrigin = function(x1,x2) 20+x1^2+x2^2-10*(cos(2*pi*x1)+cos(2*pi*x2))
x1 = seq(-5,5,by=0.1)
x2 = seq(-5,5,by=0.1)
f = outer(x1,x2,rastrigin)
persp(x1,x2,f,theta=45)
library('plot3D')
persp3D(x1,x2,f,theta=50,phi=20)
persp3D(x1,x2,f,theta=45,phi=-10)

# 최소값 찾기
library(GA)
ga = ga(type = "real-valued", fitness  = function(x) - rastrigin(x[1],x[2])  # 재귀함수
        ,lower = c(-5,-5), upper = c(5,5), popSize = 1000,maxiter = 100, pcrossover = 0.8, 
        pmutation = 0.1)
summary(ga)
plot(ga) # GA 함수 수렴 시각화
ga@solution # 최소값을 만족시키는 x1, x2
rastrigin(ga@solution[1], ga@solution[2]) # 최소목적함수값

