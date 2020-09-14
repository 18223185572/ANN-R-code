library(AMORE)
library(vegan)
metadata <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data.csv", header=T, row.names=1)
results<-metadata[,1]#第1列划分出来作为学习结果
results<-as.matrix(results)
features<-metadata[,2:38]#第2到38列为特征
features<-features+1
p0<-decostand(features,'range') #apply(date, 2, function (x) (max(x)-x)/(max(x)-min(x)))#将特征进行极差(0-1)标准化
t<-results[1:49,]#前33行特征的目标结果
t50<-results[50,]#第34行特征的目标结果
t3550<-results[1:50,]#36到50行的目标结果
t0=(t-min(t))/(max(t)-min(t))
alter=1
count=0

#训练的结果测试第9行若误差在3%之内或者循环20次结束
while(abs(alter)>0.03 && count<20){
    #训练网络，n.neurons表示输入的参数，以及隐藏层个数，及输出结果
  net<-newff(n.neurons = c(37,37,2,1),learning.rate.global=1e-4, momentum.global=0.05,error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm")
  #<span style="line-height: 27.2px; font-family: 'Helvetica Neue', Helvetica, Tahoma, Arial, STXihei, 'Microsoft YaHei', 微软雅黑, sans-serif;">p0[1:8,]表示输入，t0[1:8]表示输出，show.step表示循环次数，n.shows表示满足结果的报告次数</span>
  result<-train(net,p0[1:49,],t0[1:49],error.criterium="LMS", report=TRUE, show.step=1000, n.shows=5)
  #测试第9行到11行
  y<-sim(result$net,p0[1:49,])
  #反归一化
  y<-y*t[49]
  #用第9行来测试训练误差，满足训练误差结束
  alter=(y[1]-t50)/t50
  
  count=count+1;
}
count
#输出第9行到11行预测的值
y

#n.neurons中第一个数是输入神经元的数量，最后是输出神经元的数量，其余的都是隐藏层神经元的数量。
#learning.rate.global全局的学习率。
#momentum.global全局的动量值(貌似是步长）。
#error.criterium误差衡量算法,如用误差平方和,选“LMS”                
#hidden.layer隐藏层激活函数。                      
#output.layer输出层激活函数。                      
#method 学习方法，如梯度下降。                      

t50<-results[1:50,]#36到50行的目标结果                   
t50<-as.matrix(t50)                      
test<-cbind(y,t34)
colnames(test)<-c("pred","real")
test<-as.data.frame(test)
library(ggplot2)
library(ggpubr)
ggplot(test, aes(x = pred, y =real)) +                     
  geom_point()
ggplot(data=test, aes(x=pred, y=real))+geom_point(color="red")+stat_smooth(method="lm",se=FALSE)+stat_cor(data=test, method = "pearson")
#stat_cor(data=dat, method = "pearson")意为用pearson相关进行相关性分析，可以自行更改方法

