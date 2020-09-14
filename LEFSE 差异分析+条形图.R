library(ggplot2)
library(tidyverse)
LDA =read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/LDA.csv", header=T)

#原始数据筛选（category,term，pval)散列，按照category，-log10(pval)排序
data<-LDA%>%select(Season,ARGs,LDA)%>%arrange(Season,desc(LDA))

#画图时改变geom_bar的自动排序
data$ARGs<-factor(data$ARGs,levels = unique(data$ARGs),ordered = T)
windowsFonts(TNM = windowsFont("Times New Roman"))
ggplot(data)+
  geom_bar(aes(x=LDA,y=ARGs,fill=Season),colour = "black",stat = 'identity')+
  coord_flip()+
theme(legend.position=c(0.93,0.95), #图例在右上方
      legend.title=element_text(size=0.001),
      legend.text=element_text(size=33),#图例尺寸
      axis.text.x=element_text(colour="black",size=22,face="italic",angle = 60), #设置x轴刻度标签的字体属性
      axis.text.y=element_text(size=33,face="italic",angle = 90), #设置x轴刻度标签的字体属性
      axis.title.x=element_text(size = 33,face="plain"), #设置x轴的标题的字体属性
      axis.title.y=element_text(size = 33,face="plain"), #设置y轴的标题的字体属性
      plot.title = element_text(size=33,face="bold",hjust = 0.5))+ #设置总标题的字体属性
ylab("LDA SCORE log(10)")+xlab("ARGs") #设置x轴和y轴的标题

godata<-as_tibble(godata)
class(godata)
LDA$pos = LDA$LDA >= 0                 
ggplot(LDA, aes(x = LDA, y = ARGs, fill = pos),ordered = T) +
  # 条形图函数：position设置为"identity"是为了避免系统因绘制负值条形而引发的警告
  geom_bar(stat = "identity", position = "identity", colour = "black", size = 0.1)+ 
  # 手动调色标尺：大于0为红，小于0为蓝
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), guide = FALSE)







