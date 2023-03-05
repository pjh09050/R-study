
# kmeans dd

data = iris
km = kmeans(data[,1:4], 3)
km$centers
dist1 = sqrt(sum((data[1,1:4] - km$centers[1,])^2))
dist2 = sqrt(sum((data[1,1:4] - km$centers[2,])^2))
dist3 = sqrt(sum((data[1,1:4] - km$centers[3,])^2))

min(dist1, dist2, dist3)


tr = iris[,1:4]
ts = iris[,1:4]
k = 3

kmeans_dd = function(tr, ts, k){
  
  km = kmeans(tr, k)
  km$centers
  
  stat_mat = matrix(0, nrow(ts),1)
  for(j in 1:nrow(ts)){
    dist_mat = matrix(0, k, 1)
    for (i in 1:k){
      dist_mat[i,] = sqrt(sum((tr[j,] - km$centers[i,])^2))
    }
    min_dist = min(dist_mat)
    stat_mat[j,1] = min_dist
  }
  return(stat_mat)
}
W
kd = kmeans_dd(iris[1:150,1:4], iris[1:150,1:4],k=3)
plot(kd, type='o')









