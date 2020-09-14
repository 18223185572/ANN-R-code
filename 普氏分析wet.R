#加载数据
micro <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/communwetsp.csv", header=T, row.names=1)
args <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/ARGswet.csv", header=T, row.names=1)
#计算两个矩阵的距离
micro1_dist <- dist(t(micro))
args1_dist <- dist(t(args))

library(ape)
library(vegan)
# make pcoas 
pcoa_m <- as.data.frame(pcoa(micro1_dist)$vectors)
pcoa_a <- as.data.frame(pcoa(args1_dist)$vectors)

# procrustes
pro <- procrustes(pcoa_m, pcoa_a)
pro_test <- protest(pcoa_m, pcoa_a, perm = 999)  #普氏分析组间数据的检验

eigen <- sqrt(pro$svd$d)
percent_var <- signif(eigen/sum(eigen), 4)*100

beta_pro <- data.frame(pro$X)
trans_pro <- data.frame(pro$Yrot)
beta_pro$UserName <- rownames(beta_pro)
beta_pro$type <- "Antibiotic resistome"
trans_pro$UserName <- rownames(trans_pro)
trans_pro$type <- "Microbiome"

colnames(trans_pro) <- colnames(beta_pro)

pval <- signif(pro_test$signif, 1)

plot <- rbind(beta_pro, trans_pro)
library(ggplot2)
library(ggpubr)
food_micro <- ggplot(plot) +
  geom_point(size = 5, alpha=0.99, aes(x = Axis.1, y = Axis.2, color = type)) +
  scale_color_manual(values = c("#F8766D", "#00BFC4")) +
  geom_line(aes(x= Axis.1, y=Axis.2, group=UserName), col = "#C77CFF", alpha = 0.99,size=0.8) +
  theme(legend.text = element_text(size=24),
        legend.title = element_text(size=0.001),
        legend.position = 'top',
        axis.text = element_text(size=28),
        axis.title = element_text(size=28),
        aspect.ratio = 1) +
  guides(color = guide_legend(ncol = 1)) +
  annotate("text", x = 0.01e+08, y = 2.7e+05, label = paste0("Wet season,p=",pval), size = 9) +
  xlab(paste0("PC 1 (",percent_var[1],"%)")) +
  ylab(paste0("PC 2 (",percent_var[2],"%)")) +
  xlim(-1000000,2500000)
food_micro_leg <- get_legend(food_micro) #得到ggplot图的图例信息
food_micro + theme(legend.position = "bottom")


panel.background = element_blank(),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
theme_classic() +





