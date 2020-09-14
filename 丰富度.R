library(ggpubr)
library(ggplot2)
library(ggExtra)
richnesslat <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/richnesslat.csv", header=T)
windowsFonts(TNM = windowsFont("Times New Roman"))
p<-ggplot(data=richnesslat, aes(x =Latitude, y =Richness, size=Richness, colour = Season))+# 拟合回归线段以及置信域(默认0.95/通过level参数可自定义))+  
  geom_point(alpha = .5) + # 使散点的透明度为0.5
  stat_cor(data=richnesslat, method = "spearman", size=8)+
  stat_smooth(method="lm")+
  theme(legend.position= "bottom",
        legend.text=element_text(colour="black",size=28),
        legend.title = element_text(colour="black",size=0),
        axis.text.x=element_text(colour="black",size=28), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=28,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 28,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 28,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=28,face="bold",hjust = 0.5))+ #设置总标题的字体属性
  ylab("Richness(type)")+xlab("Latitude") #设置x轴和y轴的标题
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)


