
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