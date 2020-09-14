library(stats)
library(vegan)
antarg <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/antarg.csv", header=T,row.names=1)
ant<-antarg[,1:22]
arg<-antarg[,23:31]
#对第一、二组指标进行中心化和标准化，用于消除数据数量级的影响
antz<-scale(ant)  #（输出序号1）
argz<-scale(arg)  #（输出序号2）
#cancor用于典型相关分析的计算及输出
ca<-cancor(antz,argz);ca  #（输出序号3）
#样本数据在典型变量下得分计算及输出
U<-as.matrix(antz)%*% ca$xcoef ; U  #xcoef为第一组指标数据的典型载荷（输出序号4）
V<-as.matrix(argz)%*% ca$ycoef ; V  #ycoef为第二组指标数据的典型载荷（输出序号5）
#画相关变量U和V的数据散点图，此处因为前面每组指标均有三个变量，故画三个
#画相关变量U1和V1为坐标的数据散点图（输出序号6）
plot(U[,1], V[,1], xlab="U1", ylab="V1")
#画相关变量U1和V1为坐标的数据散点图
plot(U[,2], V[,2], xlab="U2", ylab="V2")
#画相关变量U1和V1为坐标的数据散点图
plot(U[,3], V[,3], xlab="U3", ylab="V3")

##典型相关系数的显著性检验
#典型相关系数检验的R程序（程序名：corcoef.test）
corcoef.test<-function(r, n, p, q, alpha=0.1)
{
  m<-length(r); Q<-rep(0, m); lambda <- 1
  for (k in m:1)
  {
    lambda<-lambda*(1-r[k]^2);
    Q[k]<- -log(lambda)
  }
  s<-0; i<-m
  for (k in 1:m)
  {
    Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
    chi<-1-pchisq(Q[k], (p-k+1)*(q-k+1))
    if (chi>alpha)
    {
      i<-k-1; break
    }
    s<-s+1/r[k]^2
  }
  i
}

#典型相关系数检验结果
#n代表训练数据样本数，p代表第一组指标数，q代表第二组指标数（输出序号7）
corcoef_test <- corcoef.test(r=ca$cor,n=20,p=22,q=9) 
corcoef.test




