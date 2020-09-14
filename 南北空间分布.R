library(ggpubr)
library(ggplot2)
library(ggExtra)
library(forcats)
abundis<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/abundis.csv", header=T)
name<-abundis$Estuary
value<-abundis$Relative
class<-abundis$Season
data <-data.frame(name,value,class)
data$name <- fct_inorder(data$name)
windowsFonts(TNM = windowsFont("Times New Roman"))
ggplot(data=data, aes(x =value, y =name, fill=class)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme(legend.position= "bottom",
        legend.text=element_text(colour="black",size=15),
        legend.title = element_text(colour="white",size=0),
        axis.text.x=element_text(colour="black",size=15), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=15,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 15,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 15,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=15,face="bold",hjust = 0.5))+ #设置总标题的字体属性
  ylab("Estuary")+xlab("Relative abundance(ppm)") #设置x轴和y轴的标题
  
  
  