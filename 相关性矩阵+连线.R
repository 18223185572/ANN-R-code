library(vegan)
library(dplyr)
library(corrplot)
par(omi = c(0.3, 0.3, 0.3, 0.3),
    cex = 1.2,
    family = 'Times New Roman') 
M <- cor(decostand(mtcars,method="hellinger",na.rm=T))#计算相关系数矩阵
corrplot(M, method = "square", type = 'upper')
head(mtcars)


# 准备数据
set.seed(20190420)
n <- ncol(mtcars)
grp <- c('Abundance', 'Richness', 'Shannon-Wiener') # 分组名称
sp <- c(rep(0.0008, 6), rep(0.007, 2), rep(0.03, 3), rep(0.13, 22)) # P值
gx <- c(-4.5, -2.5, 1) # 分组的X坐标
gy <- c(n-1, n-5, 2.5) # 分组的Y坐标
df <- data.frame(
  grp = rep(grp, each = n), # 分组名称，每个重复n次
  gx = rep(gx, each = n), # 组X坐标，每个重复n次
  gy = rep(gy, each = n), # 组Y坐标，每个重复n次
  x = rep(0:(n - 1) - 0.5, 3), # 变量连接点X坐标
  y = rep(n:1, 3), # 变量连接点Y坐标
  p = sample(sp), # 对人工生成p值进行随机抽样
  r = sample(c(rep(0.8, 4), rep(0.31, 7), rep(0.12, 22))) 
  # 对人工生成r值进行随机抽样
)

length(rep(grp, each = n))
length(rep(gx, each = n))
length(rep(gy, each = n))
length(rep(0:(n - 1) - 0.5, 3))
length(rep(n:1, 3))
length(sample(sp))
length(sample(c(rep(0.8, 4), rep(0.31, 7), rep(0.12, 22))) )

# 这一部分代码是按照原图图例说明处理线条宽度和颜色映射
df <- df %>% 
  mutate(
    lcol = ifelse(p <= 0.001, '#1B9E77', NA), 
    # p值小于0.001时，颜色为绿色，下面依次类推
    lcol = ifelse(p > 0.001 & p <= 0.01, '#88419D', lcol),
    lcol = ifelse(p > 0.01 & p <= 0.05, '#A6D854', lcol),
    lcol = ifelse(p > 0.05, '#B3B3B3', lcol),
    lwd = ifelse(r >= 0.5, 14, NA),
    # r >= 0.5 时，线性宽度为14，下面依次类推
    lwd = ifelse(r >= 0.25 & r < 0.5, 7, lwd),
    lwd = ifelse(r < 0.25, 1, lwd))
    

segments(df$gx, df$gy, df$x, df$y, lty = 'solid', lwd = df$lwd,col = df$lcol, xpd = TRUE) # 绘制连接线
points(gx, gy, pch = 24, col = 'blue', bg = 'blue', cex = 3, xpd = TRUE) 
 # 组标记点
text(gx - 0.5, gy, labels = grp, adj = c(1, 0.5), cex = 1.5, xpd = TRUE)
    # 组名称

labels01 <- c('<= 0.001','0.001 < x <= 0.01','0.01 < x <= 0.05','> 0.05')
labels02 <- c('>= 0.5', '0.25 - 0.5', '< 0.25')
labels_x <- rep(-6, 4)
labels_y <- seq(4.6, 2.6, length.out = 4)
text(-6.5, 5.2, 'P-value', adj = c(0, 0.5), cex = 1.2, font = 2, xpd = TRUE)
text(labels_x, labels_y, labels01, adj = c(0, 0.5), cex = 1.2, xpd = TRUE)
points(labels_x - 0.5, labels_y, pch = 20, col = c('#1B9E77', '#88419D','#A6D854', '#B3B3B3'),
           cex = 3, xpd = TRUE)
lines_x <- c(-6.5, -3, 0.5)
lines_y <- rep(1.2, 3)
text(-6.5, 1.9, "Mantel's r", adj = c(0, 0.5), cex = 1.2, font = 2, xpd = TRUE)
text(lines_x + 1.5, lines_y, labels02, adj = c(0, 0.5), cex = 1.2, xpd = TRUE)
segments(lines_x, lines_y, lines_x + 1, lines_y, lwd = c(14, 7, 2.5), lty = 'solid', 
             col = '#B3B3B3', xpd = TRUE)
    
## 图例框框
    segments(-6.9, 5.6, -2.8, 5.6, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    segments(-2.8, 5.6, -2.8, 1.8, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    segments(-2.8, 1.8, 3.6, 1.8, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    segments(3.6, 1.8, 3.6, 0.7, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    segments(3.6, 0.7, -6.9, 0.7, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    segments(-6.9, 0.7, -6.9, 5.6, lty = 'solid', lwd = 1.2, 
             col = 'grey50', xpd = TRUE)
    
    
    
