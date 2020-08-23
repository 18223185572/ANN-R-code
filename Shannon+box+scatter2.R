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
plotdata <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/Rcode/plotdata.csv", header=T)
library(ggplot2) #调用ggplot软件包
library(ggpubr)
#使用ggplot2包生成箱线图
p <- plotdata %>% 
  ggplot(aes(Group, Shannon)) + 
  geom_violin(aes(fill = Group),trim = F) + 
  stat_boxplot(geom = 'errorbar', width = 0.1) + 
  geom_boxplot(aes(fill = Group), width = 0.2, show.legend = F) +
  geom_jitter(aes(shape = Group)) + 
  stat_summary(fun.y = median, geom = 'point', color = 'white', size = rel(2)) + 
  guides(shape = F, color = F) + 
  theme_bw() + 
  scale_fill_brewer(palette = 'Set2')
mycomparision <- list(c('Wet season', 'Dry season'))  #设置比较组
bartlett.test(Shannon ~ Group)
#Bartlett test of homogeneity of variances
p1 <- p + stat_compare_means(method = 'wilcox.test', comparisons = mycomparision, label = 'p.signif') #以显著性（星星）显示结果
p1
