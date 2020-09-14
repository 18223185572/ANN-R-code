library(ggplot2) #调用软件包
library(ggpubr)
abundance <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/absoluteabundance.csv", header=T)
compaired <- list(c("Dry season", "Wet season"))
windowsFonts(TNM = windowsFont("Times New Roman"))
ggplot(abundance,aes(x=Season,y=Absolute,fill=Season))+ #”fill=“设置填充颜色
  geom_boxplot(size=0.5,fill="white",width=0.4, outlier.fill="white",outlier.color="white")+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
  geom_jitter(aes(fill=Season),width =0.2,shape = 21,size=8)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
  stat_compare_means(comparisons=compaired,size=8)+ # Add pairwise comparisons p-value 
    theme(legend.position="none", #不需要图例
        axis.text.x=element_text(colour="black",size=27), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=28,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 28,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 28,face="plain"), #设置x轴的标题的字体属性
        plot.title = element_text(size=28,face="bold",hjust = 0.5))+ #设置总标题的字体属性
  ylab("Absolute abundance(copies/g)")+xlab("Seasonal variation")+ #设置x轴和y轴的标题
    labs(x=NULL)   #可自定义标签名字

library(ggplot2)

