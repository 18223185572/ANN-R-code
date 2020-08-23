rm(list=ls())
#setwd("")  #设置路径

#随机选择Z折下标集的函数，n样本量，seed随机种子
CV=function(n,Z=10,seed=888){
  z=rep(1:Z,ceiling(n/Z))[1:n]
  set.seed(seed)
  z=sample(z,n)
  mm=list()
  #mm[[i]]为第i个下标集
  for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

#数据导入
w <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data.csv", header=T, row.names = 1)
n=nrow(w);Z=10;mm=CV(n,Z);D=1
library(e1071)
library(randomForest)
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z){
  m=mm[[i]];
  M=mean((w[m,D]-mean(w[m,D]))^2)
  a=svm(ARGs~.,w[-m,])
  a=randomForest(ARGs~.,data=w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

