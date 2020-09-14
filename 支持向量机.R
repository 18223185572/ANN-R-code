library(e1071)
datatotal <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/datatotalfeat.csv", header=T,row.names=1)
x <- subset(datatotal, select = -ARGs)
y <- ARGs
sv<- svm(x, y) 
ypre<-predict(sv,x)
real<-y
pred<-ypre
test<-as.data.frame(cbind(y,ypre))
ggplot(test, aes(x=pred, y=real))+# 拟合回归线段以及置信域(默认0.95/通过level参数可自定义))+  
  geom_point(alpha = .4,colour="#F8766D",size=8) + # 使散点的透明度为0.5
  scale_size_continuous( trans="identity")+ 
  stat_smooth(method="lm",colour="#F8766D")+
  stat_cor(data=test2, method = "spearman", size=7)+
    theme(legend.position= "bottom",
        legend.text=element_text(colour="black",size=24),
        legend.title = element_text(colour="black",size=0),
        axis.text.x=element_text(colour="black",size=24,angle=90), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(colour="black",size=24), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 24), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 24), #设置x轴的标题的字体属性
        plot.title = element_text(size=24,hjust = 0.5))+ #设置总标题的字体属性
  ylab("Real abundance(ppm)")+xlab(" Predicted abundance(ppm)") #设置x轴和y轴的标题

