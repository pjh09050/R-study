
secant = function(f,init,tol = 1e-9,max = 100){
  i = 0
  oldx = init
  oldfx = f(init)     #초기값에 따른 y값
  x = oldx +10*tol    # 밑에 조건 참고
  
  # convergence
  while(abs(x-oldx)>tol){
    i = i+1
    
    if(i>max) stop("there is no solution")
    fx = f(x)                        # 업데이트된 y
    newx = x-f(x)*((x-oldx)/(fx-oldfx))  # secant, 할선의 방정식 (즉, y=0을 지나는 x 찾기)
    oldx = x                                # 기존 oldX 는 x로 바꿔주고
    oldfx = fx                               # oldfx도 fx로 바꿔주고
    x = newx                                 # x는 newx로 업데이트
    cat("iteration",i,"value of x is:", x,"\n")
  }
  return(x)
}
f = function(x) exp(-x)-x
x
