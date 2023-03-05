
df = iris[,1:4]
df
df = as.matrix(scale(df))

## covariance Martrix
## sigma(xi %*% t(xi))

result = matrix(0,4,4)
for(i in 1:nrow(df)){
  tmp = df[i,]%*%t(df[i,])
  result = result + tmp
}

cova1 = result/nrow(df)
mat = cov(df)


eig = eigen(mat)
eig$values
eig$vectors

# 정보의 양
det(mat - (eig$values * diag(4)))

# 주성분의 설명 양
pc = princomp(df)
plot(pc$scores[,1], pc$scores[,2])
biplot(pc)

# 위에꺼는 W
pc$loadings

# 람다
pc$sdev

summ = summary(pc)
screeplot(pc)
