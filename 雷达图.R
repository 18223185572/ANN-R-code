#第一步：设置各个变量的取值范围（即最小值和最大值，相当于二维图的x轴和y轴的取值范围），代码如下
#取值范围为 先最大值 后最小值，即上述c（1,0）
maxmin <- data.frame(
  AR=c(1, 0),
  AS=c(0.5, 0),
  AT=c(0.1, 0),
  ABS=c(0.1, 0))
#第二步：设置准备绘图的指标类型的数据，如 A 种鱼的5种饵料贡献率
dat.A<- data.frame(
  AR=c(0.674, 0.628),
  AS=c(0.233, 0.287),
  AT=c(0.079,0.070),
  ABS=c(0.014, 0.016))
#第三步：把数据集 maxmin 和 dat.A 组合成为新的数据集，命名为dat.A2
dat.A2<-rbind(maxmin,dat.A)
#第四步: 绘图
#如何没有安装套件的话，请先安装fmsb套件，代码为 install.packages('fmsb')
library(fmsb)
library(ggplot2)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE)
#seg的取值表示将各轴分成几段，centerzero表示蜘蛛网图中心点为最小值点，axistype为0表示不绘制各坐标轴标签




