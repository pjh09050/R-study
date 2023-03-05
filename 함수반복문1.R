# 함수 만들기
# 1부터 N까지 더하기
sum1toN = function(N){
  result = sum(1:N)
  return (result)
}
sum1toN(100)


# x,y,z값을 제곱하고 서로 더한 후 root 취하기
sqrt_sum = function(x,y,z){
  result =sqrt(x^2 + y^2 + z^2)
  return(result)
}
sqrt_sum(4, 55, 14)

# 첫번째 함수와 두번째 함수를 같이 써보기
sqrt_sum2 = function(x,y,z){
  result = sqrt(x^2 + y^2 + z^2)
  result2 = sum1toN(as.integer(result))
  return(result2)
}
sqrt_sum2(1,2,3)

a = 56.89464
as.integer(a)


#------------------------------------------

# 반복문
for (i in 1:10){print(i)}

number = 1
while(number <= 10){
  print(number)
  number = number + 1
}

# 3x3 배열 2개 만듬
b = array(1:18, dim = c(3,3,2))
print(b)

apply(b,3,diag) # diag 대각행렬


A = matrix(1:12, nrow=4 ,byrow = T)
print(A)
B = matrix(1:12, nrow=4 ,byrow = F)
print(B)

# 각 배열에 대해 원하는 연산/ 추출 가능
# apply 1은 row, 2는 column 방향
apply(A, 1, FUN=sum)
apply(A, 2, FUN=sum)



