dev.off()
library(fmsb) 
data<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/radarcharm.csv", header=T,row.names=1)
colnames(data)<-c("AR", "AS", "AT", "ABS")#为矩阵a的添加列名称
# 用于生成雷达图的最大最小值
data=rbind(rep(1,4) , rep(0,4) , data)
colors_border=c("#F8766D","#00BFC4")

windowsFonts(TNM = windowsFont("Times New Roman"))
radarchart( data  , 
axistype=1 , #坐标轴类型
pcol=colors_border , #数据边框颜色
plwd=4 , #网格线宽
plty=1,#网格数据型
cglcol="navy",cglty=1, #网格线型
axislabcol="#000000",#轴线颜色
caxislabels=seq(0,0.8,4),#轴线标签
cglwd=0.8,
vlcex=0.8 
)+
legend(x=0.4, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "#000000", cex=1.2, pt.cex=3)

par(bg=background)




