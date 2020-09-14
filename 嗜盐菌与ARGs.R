library(ggpubr)
library(ggplot2)
library(ggExtra)
saliarg<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/saliarg.csv", header=T)
windowsFonts(TNM = windowsFont("Times New Roman"))
p<-ggplot(data=saliarg, aes(x =ARGs, y =Abundance, size=Abundance,colour = Class))+# 拟合回归线段以及置信域(默认0.95/通过level参数可自定义))+  
  geom_point(alpha = .4) + # 使散点的透明度为0.5
  scale_size_continuous( trans="identity", range=c(3, 11))+ 
  stat_smooth(method="lm")+
  stat_cor(data=saliarg, method = "spearman", size=5)+
  theme(legend.position= "bottom",
        legend.text=element_text(colour="black",face="plain",size=15),
        legend.title = element_text(colour="black",face="plain",size=0),
        axis.text.x=element_text(colour="black",face="plain",size=15), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=15,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 15,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 15,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=15,face="plain",hjust = 0.5))+ #设置总标题的字体属性
  ylab("Halobacteria abundance(reads)")+xlab(" ARGs abundance(ppm)") #设置x轴和y轴的标题
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
a