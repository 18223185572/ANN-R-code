#加载数据
data <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data.csv", header=T, row.names=1)
# 加载包
library(vegan)
#极差(0-1)标准化
data1<-decostand(data,'range')#apply(data1, 2, function (x) (max(x)-x)/(max(x)-min(x)))
# 加载包
library(bnlearn)
# 数据离散化
data2 <- discretize(data1[,-1],method = "interval", breaks = 3)
data2$class <- data1[,1]
# 使用爬山算法进行结构学习
bayesnet <- hc(data2)
# 显示网络图
plot(bayesnet)
# 修改网络图中的箭头指向
bayesnet<- set.arc(bayesnet,'age','pregnant')
# 参数学习
fitted <- bn.fit(bayesnet, data2,method='mle')
# 训练样本预测并提取混淆矩阵
pre <- predict(fitted,data=data2,node='class')
confusionMatrix(pre,data2$class)
# 进行条件推理
cpquery(fitted,(class=='pos'),(age=='(36,81]'&mass=='(34.8,67.1]'))
