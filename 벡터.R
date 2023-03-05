x = c(1,3,5)
y = c(2,-1,0)
x+y
#install.packages("matlib")
library(matlib)

# 플롯 기본 셋팅
xlim <- c(0,6)
ylim <- c(0,6)
plot(xlim, ylim, type='n', xlab='x1', ylab='x2', asp=0)
grid()

# define some vectors
a = c(4,2)
b = c(1,3)

# 벡터 플롯팅 (vectors 함수 활용)
vectors(b, labels='b', pos.lab=4, frac.lab=5, col='green')
vectors(a, labels='a', pos.lab=4, frac.lab=5)
vectors(a+b, labels='a+b', pos.lab=4, frac.lab=5, col='red')

# x가 (3,5), y가 (1,2) 일 때 빗변의 길이
x = c(3,5)
y = c(1,2)
y_x = sqrt((3-1)^2 + (5-2)^2)
y_x

# 벡터의 내적
a = c(2,4)
b = c(3,1)
a%*%b
# 빗변의 길이
sqrt(t(a)%*%b)
sqrt(a%*%b) # 빗변의 길이 구하는 것과 내적해서 sqrt 해주는 것이 똑같다.

# 벡터의 각
a = c(2,4)
b = c(3,1)
a%*%b/sqrt(a%*%a) %*% sqrt(b%*%b) # 0이면 관계가 없다.

res = matrix(0,980, 1)
  for (i in 1:980){
    res[i,1] = cos(i*pi/180)
  }
  plot(res,type="l")
which(res<=0.7)

#------------------------------------

# 행렬
a = matrix(c(1,4,2,5,3,6), ncol=3)
a
b = matrix(c(0,2,1,-1,0,5),2,3)
b
a + b

c = matrix(c(1,2,0,5), 2,2)
c
2 * c

A = matrix(c(2,0,1,1,3,-1), ncol=3)
A
# 전치
t(A)

mat = matrix(c(2,4,3,1,3,2,4,5,1,2,3,4,1,1,2,4), nrow=4)
mat
det(mat) # 행렬식 |mat| volume을 의미
solve(mat) # 역행렬
inv(mat)

# 벡터의 노름
# install.packages("pracma")
library(pracma)
v = c(1,2,1)
Norm(v,p=2) # p=2 L2 norm 유클리디안 거리 sqrt(6)
p = matrix(c(2,4,3,2), ncol=2)
Norm(p,p=1)

#고유값과 고유벡터
mat = matrix(c(5,25,35,25,166,175,35,175,325), ncol=3)
mat
enanlysis = eigen(mat, symmetric=T)
enanlysis
enanlysis$values
vec = enanlysis$vectors
t(vec)%*%vec # 대칭행렬이 된다.

det(mat)
enanlysis$values[1]*enanlysis$values[2]*enanlysis$values[3]
prod(enanlysis$values)

mat = matrix(c(1,1,0,3), ncol=2)
mat
eanalysis = eigen(mat, symmetric = F)  # 대칭 행렬이 아니므로 symmetric = F
eanalysis
prod(eanalysis$value)
det(mat)










