参考论文：
1 Kernel Density-Based Linear Regression Estimate

2 Weighted LAD-LASSO method for robust parameter estimation 
  and variable selection in regression
  
#############################################################################
编程思路：

1.设计编写IRIS函数，求而出beta；

2.根据lambda和beta关系可求出lambda；

3.重新构造数据集，对新的数据集进行最小二乘估计，求出新的beta。


###step1
beta0 = lm(y~x,data=data)$coef
normal<-function(beta0,delta){
  n = nrow(x)
  p = ncol(x)
  RD = t(y-t(x)*beta0-e)%*%var(x)%*%(y-t(x)*beta0-e)
  w = p/RD
  w = as.matrix(w)
  times = 1
  if(abs(w) <= 1){
  beta1 = solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%y
  
  while(abs(beta1-beta0)>delta and times<len(nrows(x)/4)){
  
    RD = t(y-t(x)*beta0-e)%*%var(x)%*%(y-t(x)*beta0-e)
    w = p/RD
    w = as.matrix(w)
    beta0 = beta1
    beta1 = solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%y 
	times = times + 1
    }
  }
  return beta1
}

###step2
lambda = 1/abs(beta1)

###step3
newdata = (x_star,y_star) = (n*lambda*diag(ncol(x)),0)
Q(beta) = lm(y_star~x_star,data=newdata)
beta = Q(beta)$coef

####模拟
epsilon = rt(1000,df=1)
x = rnorm(1000,0,1)
y = 1+2*x+epsilon
data = data.frame(x,y)
yhat = lm(y~x,data=data)
e = predict(yhat)
beta0 = yhat$coef
x = as.matrix(x)
