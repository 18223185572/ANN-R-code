metadata <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/PCoA2.csv", header=T)
iris<-metadata
iris <- data.frame(iris)
iris_sub <- iris[,2:235]
group <- as.factor(iris$Species)

mds <- cmdscale(dist(iris_sub), k = 3, eig = TRUE)
mds_point <- data.frame(mds$points)   # 得到各样本的坐标
colnames(mds_point) <- c('X1','X2')
eig <- mds$eig

library(ggpubr)
a<-iris[,1]
mds_point <- cbind(mds_point, a)
colnames(mds_point)[4] <- "group"
ggscatter(mds_point, x= "X1", y = "X2", 
          color = "group",  # 任何存在的palette都可以，不仅仅是brewer.pal中的，可以调用ggsci中的各种sci-fi主题颜色包！
          ellipse = TRUE,  # 设置是否需要confidence ellipses
          mean.point = TRUE, star.plot = TRUE,   # 设置confidence ellipses中心是否与所有点连线
          ellipse.level = 0.95,  # 设置confidence level可以调整椭圆的大小
          ggtheme = theme_minimal())+
  labs(x = paste("PCoA 1 (", format(100*eig[1]/sum(eig), digits = 4), "%)",sep = ""), 
       y = paste("PCoA 2 (", format(100*eig[2]/sum(eig), digits = 4), "%)",sep = "")) +
  ggtitle("PcoA")


library(vegan)
grou <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/group.csv", header=T)
PERMANOVA <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/PCoA.csv", header=T, row.names = 1)
dis1 <- vegdist(PERMANOVA, method = 'bray')
adonis_result_dis1 <- adonis(dis1~group, grou, permutations = 999)     #同上所述
adonis_result_dis1

