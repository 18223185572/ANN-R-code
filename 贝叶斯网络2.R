library(randomForest)
library(recipes)
library(Rgraphviz)
library(bnlearn)
datatotal <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/datatotalfeat.csv", header=T,row.names=1)
bn.pc<-pc(datatotal)

datatotal<-matrix(datatotal)
data5 <- discretize(x=datatotal,cuts=4)  
data2$class <- data3[,14]  
# 使用爬山算法进行结构学习 
bayesnet <- hc(data5) 
# 显示网络图  
plot(bayesnet) 

random<-randomForest(ARGs~.,data=datatotal,importance=TRUE, ntree=1000)
print(random)
#自变量的重要程度
importance(random)
#自变量重要性排序图
varImpPlot(random)

install.packages('BiocManager', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN')
options('BioC_mirror'='https://mirrors.tuna.tsinghua.edu.cn/bioconductor/')
BiocManager::install(c('graph','RBGL','Rgraphviz'))
install.packages(c('kpcalg'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN')
library(pcalg)
suffStat <-list(C = cor(datatotal), n = nrow(datatotal))
pc.fit <- pc(suffStat, indepTest = gaussCItest,
             alpha = 0.01,labels =names(datatotal),verbose = TRUE)
pc.fit
plot(pc.fit)
