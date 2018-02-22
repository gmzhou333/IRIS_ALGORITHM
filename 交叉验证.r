################LOOCV 

x=Auto$horsepower;
y=Auto$mpg;
data=cbind(x,y);
MSE1<-0;
MSE2=0;
for(i in 1:length(y)){
  data1=data.frame(data[-i,]);
  lm1=lm(y~x,data=data1);
  lm2=lm(y~x+I(x^2),data=data1);
  MSE1[i]=(y[i]-predict(lm1,data.frame(x=x[i]),se.fit=TRUE)$fit)^2;
  
  MSE2[i]=(y[i]-predict(lm2,data.frame(x=x[i]),se.fit=TRUE)$fit)^2;
  
}
mean(MSE1)
mean(MSE2)

##################K-CV
K=10;
x=Auto$horsepower;
y=Auto$mpg;
data=cbind(x,y);
MSE1<-0;
MSE2=0;
index=sample(1:length(y))
for(i in 1:K){
  if(i<=K-1){
    data1=data.frame(data[-c(index[((i-1)*39+1):(39*i)]),]);
    lm1=lm(y~x,data=data1);
    lm2=lm(y~x+I(x^2),data=data1);
    MSE1[i]=mean(y[index[((i-1)*39+1):(39*i)]]-predict(lm1,data.frame(x=index[((i-1)*39+1):(39*i)]),se.fit=TRUE)$fit)^2;
    
    MSE2[i]=mean(y[index[((i-1)*39+1):(39*i)]]-predict(lm2,data.frame(x=index[((i-1)*39+1):(39*i)]),se.fit=TRUE)$fit)^2;
  }
  else{
    
    data1=data.frame(data[-c(index[((i-1)*39+1):(392)]),]);
    lm1=lm(y~x,data=data1);
    lm2=lm(y~x+I(x^2),data=data1);
    MSE1[i]=mean(y[index[((i-1)*39+1):(392)]]-predict(lm1,data.frame(x=index[((i-1)*39+1):(392)]),se.fit=TRUE)$fit)^2;
    
    MSE2[i]=mean(y[index[((i-1)*39+1):(392)]]-predict(lm2,data.frame(x=index[((i-1)*39+1):(392)]),se.fit=TRUE)$fit)^2;
  }
}

mean(MSE1)
mean(MSE2)

