
A = c(50,70)
B = c(70,80)
C = c(50,90)
D = c(80,60)
E = c(90,95)
F = c(50,55)

univs = rbind(A,B,C,D,E,F) 
print(univs)

plot(univs,xlab = "입학성적", ylab = "취업률", xlim = c(40, 100), ylim = c(40, 100), 
     main = "scatter plot of final scores")

text(univs[,1], univs[,2], labels = abbreviate(rownames(univs)), 
     cex = 0.8, pos = 1, col = "blue")

# 유클리디안 거리 구하기
eucl = dist(univs, method='euclidean')
print(eucl)

# A와 C의 거리만 구하기
eucl = dist(univs[c(1,3),], method = 'euclidean')
print(eucl)

# A와 B의 거리만 구하기
eucl = dist(univs[c(1,2),], method = 'euclidean')
print(eucl)

# 수학공식으로 A와 B의 거리 구하기
sqrt(t(A-B)%*%(A-B))


# 맨하탄 거리
A-B
sum(abs(A-B))


# 마할라노비스 거리
xbar = as.vector(colMeans(univs[c(1,2,5,6),]))

sqrt(t(xbar-C)%*%(xbar-C)) # 유클리디안 거리

cova = cov(univs[c(1,2,5,6), ])
# dim(cova) # 차원 확인
xbar
cova

sqrt(t(C-xbar)%*%solve(cova)%*%(C-xbar))
sqrt(t(D-xbar)%*%solve(cova)%*%(D-xbar))


