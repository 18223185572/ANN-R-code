data <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data.csv", header=T, row.names = 1)
library(vegan)
Shannon.Wiener <- diversity(data, index = "shannon")#或计算香农指数
#Simpson <- diversity(herb.mat, index = "simpson")#或计算辛普森指数
#Inverse.Simpson <- diversity(herb.mat, index = "inv")#或计算Inverse Simpson指数
#S <- specnumber(herb.mat)#或计算物种累计数
#J <- Shannon.Wiener/log(S)#或计算Pielou均匀度指数
Shannon.Wienerplot<-as.matrix(Shannon.Wiener)#将计算后的数据转为矩阵
colnames(Shannon.Wienerplot)<-c("Shannon")#修改列名为Shannon
row.names(Shannon.Wienerplot)<-c("Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Wet season",	"Dry season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season",	"Wet season",	"Dry season")
Group <- rownames(Shannon.Wienerplot)#提取行名称
rownames(Shannon.Wienerplot) <- NULL#行名称设置为无
plotdata <- cbind(Group,Shannon.Wienerplot)#将行名称设置为第一列数据
write.table(plotdata,"C:/Users/Robin.DESKTOP-0U5O684/Desktop/Rcode/plotdata.csv",row.names=FALSE,col.names=TRUE,sep=",")
data <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/Rcode/plotdata.csv", header=T)
library(ggplot2) #调用ggplot软件包
library(ggpubr)
compaired <- list(c("Dry season", "Wet season"))
#使用ggplot2包生成箱线图
ggplot(data,aes(x=Group,y=Shannon,fill=Group))+ #”fill=“设置填充颜色
geom_boxplot(size=0.5,fill="white",width=0.4, outlier.fill="white",outlier.color="white")+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
geom_jitter(aes(fill=Group),width =0.2,shape = 21,size=5)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
scale_fill_manual(values = alpha(c("#F8766D", "#619CFF"),0.3))+  #设置填充的颜色
scale_color_manual(values=alpha(c("#F8766D","#619CFF"),0.6))+ #设置散点图的圆圈的颜色为黑色
stat_compare_means(comparisons=compaired)+ # Add pairwise comparisons p-value 
stat_compare_means(label.y = 1.5)+ # Add global p-value+
theme_bw()+ #背景变为白色
theme(legend.position="none", #不需要图例
axis.text.x=element_text(colour="black",size=14), #设置x轴刻度标签的字体属性
axis.text.y=element_text(size=14,face="plain"), #设置x轴刻度标签的字体属性
axis.title.y=element_text(size = 14,face="plain"), #设置y轴的标题的字体属性
axis.title.x=element_text(size = 14,face="plain"), #设置x轴的标题的字体属性
plot.title = element_text(size=15,face="bold",hjust = 0.5), #设置总标题的字体属性
panel.grid.major = element_blank(), #不显示网格线
panel.grid.minor = element_blank())+
ylab("Shannon-Wiener")+xlab("Seasonal variation")+ #设置x轴和y轴的标题
theme(text=element_text(family = 'Times'))
theme_bw(base_family = 'Times')
