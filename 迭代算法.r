##二分法##
Bi<-function(a,b,f,delta){
n=round(log2((b-a)/delta));
for(i in 1:n){
m=(a+b)/2;
if(f(m)==0){
break;
}
else if(f(a)*f(b)>0){
a=m;
}
else{
b=m;
}
}
list(root=m);
}

f=function(x) x*sin(x)-1
Bi(0,2,f,1e-6)

##牛顿函数迭代##
Newton<-function(int,f,det,delta){
x0=int;
x1=x0-f(x0)/det(x0);
it=1;
while(abs(x1-x0)>delta){
it=it+1;
x0=x1;
x1=x0-f(x0)/det(x0);
}
list(root=x1,it=it);
}

f=function(x) x^3-x-3
det=function(x) 3*x^2-1
Newton(3,f,det,1e-6)

##分位数回归##
data=rnorm(1000,0,1);
MM<-function(int,data,delta){
theta0=int;
theta1=sum(data/abs(data-theta0))/(sum(1/abs(data-theta0)));
while(abs(theta1-theta0)>delta){
theta0=theta1;
theta1=sum(data/abs(data-theta0))/(sum(1/abs(data-theta0)));
}
list(root=theta1);
}


MM(mean(data),data,1e-6)

##模拟##
B=200;
result1=matrix(0,ncol=2,nrow=B)
result2=matrix(0,ncol=2,nrow=B)

for(i in 1:B){
x=rnorm(1000,0.1);
epsilon=rt(1000,df=1);
y=1+2*x+epsilon;
data=data.frame(x,y);
ls=lm(y~x,data=data)
result1[i,]=ls$coef
MM=rlm(y~x,data=data,method="MM")
result2[i,]=MM$coef
print(i)
}


apply(result1,2,mean)
apply(result1,2,sd)
apply(result2,2,mean)
apply(result2,2,sd)













