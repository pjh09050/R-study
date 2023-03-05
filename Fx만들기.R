## fx 만들기

sum1toN = function(N){
  result = sum(1:N)
  return(result)
}

sum1toN(10)

f1 = function(x,y){
  result = x^2 + cos(y) + x/y
  return(result)
}

f1(10,20)


result_mat = matrix(0,100,1)

for(i in 1:100){
  result = (i^2 + cos(i) + 0.33)
  result_mat[i,1] = result
}
plot(result_mat[,1])

i=1
while(i <10){
  print(i)
  i = i+1
}

df = iris
names(df)
str(df)
head(df)
summary(df)

plot(df[,1:4])

head(df)
appl = apply(df[,1:4],1,sum) # margin이 1은 row 방향, 2는 column 방향

nrow(df)
length(appl)
ncol(df)

appl = apply(df[,1:4],2,sum)
length(appl)

