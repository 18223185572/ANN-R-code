library(corrplot)
library(graphics)
MGEscorr <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/MGEscorr.csv", header=T)
MGE<-cor(MGEscorr,method="spearman")
res1 <- cor.mtest(MGEscorr, conf.level = .95)
res2<-res1$p[,11:19]#提取显著性矩阵11-19列
res3<-res2[1:10,]#提取显著性矩阵1-10行
newma<-MGE[,11:19]#提取相关性矩阵11-19列
newma2<-newma[1:10,]#提取相关性矩阵1-10行
corrplot(newma2, method="pie",p.mat = res3, insig = "label_sig",col= c("#00B9E3","#00BFC4","#00C091","white","#EA8331","#F8766D", "#FF6C91"),
         sig.level = c(.001, .01, .05), pch.cex = 2, pch.col = "black",tl.cex=2, tl.col="black")

library(scales)
show_col(hue_pal()(40)) #加载颜色，n为1~6
