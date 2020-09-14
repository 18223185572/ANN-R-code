metadata <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/PCoA0827.csv", header=T)
iris<-metadata
iris <- data.frame(iris)
iris_sub <- iris[,2:651]
group <- as.factor(iris$Species)

mds <- cmdscale(dist(iris_sub), k = 3, eig = TRUE)
mds_point <- data.frame(mds$points)   # 得到各样本的坐标
colnames(mds_point) <- c('X1','X2')
eig <- mds$eig
library(ggExtra)
library(ggplot2)
library(ggpubr)
a<-iris[,1]
mds_point <- cbind(mds_point, a)
colnames(mds_point)[4] <- "group"
windowsFonts(TNM = windowsFont("Times New Roman"))
p<-ggscatter(mds_point, x= "X1", y = "X2", 
          size = 7,
          color = "group",  # 任何存在的palette都可以，不仅仅是brewer.pal中的，可以调用ggsci中的各种sci-fi主题颜色包！
          ellipse = FALSE,  # 设置是否需要confidence ellipses
          mean.point = TRUE, star.plot = TRUE,   # 设置confidence ellipses中心是否与所有点连线
          star.plot.lwd=1,
          ggtheme =  theme_gray())+
          labs(x = paste("PCoA 1 (", format(100*eig[1]/sum(eig), digits = 4), "%)",sep = ""), 
       y = paste("PCoA 2 (", format(100*eig[2]/sum(eig), digits = 4), "%)",sep = ""))+
   theme(legend.position="bottom", #图例位于底部
      legend.text=element_text(size=26),#图例尺寸
      legend.title = element_text(colour="white",size=0),
      axis.text.x=element_text(colour="black",size=26), #设置x轴刻度标签的字体属性
      axis.text.y=element_text(size=26,face="plain"), #设置x轴刻度标签的字体属性
      axis.title.x=element_text(size = 26,face="plain"), #设置x轴的标题的字体属性
      axis.title.y=element_text(size = 26,face="plain"), #设置y轴的标题的字体属性
      plot.title = element_text(size=26,face="bold",hjust = 0.5))
   ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
   


library(vegan)
grou <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/group.csv", header=T)
PERMANOVA <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/PCoA.csv", header=T, row.names = 1)
dis1 <- vegdist(PERMANOVA, method = 'bray')
adonis_result_dis1 <- adonis(dis1~group, grou, permutations = 999)     #同上所述
adonis_result_dis1

