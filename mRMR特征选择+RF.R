SO2浓度+NO2浓度+可吸入颗粒浓度+森林覆盖率+人均公共绿地面积+工业COD排放量+生活COD排放量+
  + 废水排放总量+人均GDP+城乡收入比+万元GDP能耗+二三产业占比+环保支出占GDP比重

NO2浓度+工业COD排放量+生活COD排放量+城乡收入比+二三产业占比+环保支出占GDP比重


train_sub = sample(nrow(wine),6/10*nrow(wine))
train_data = wine[train_sub,]
test_data = wine[-train_sub,]
library(pROC) #绘制ROC曲线
library(randomForest)
#数据预处理
train_data$等级 = as.factor(train_data$等级)
test_data$等级 = as.factor(test_data$等级)
wine_randomforest <- randomForest(等级 ~  SO2浓度+NO2浓度+可吸入颗粒浓度+森林覆盖率+人均公共绿地面积+工业COD排放量+生活COD排放量+废水排放总量+人均GDP+城乡收入比+万元GDP能耗+二三产业占比+环保支出占GDP比重,
                                    data = train_data,
                                    ntree =500,
                                    mtry=3,
                                    importance=TRUE ,
                                    proximity=TRUE)
#查看变量的重要性
wine_randomforest$importance
#0：表示变量替换后对分类为0的数据的影响；
#1：表示变量替换后对分类为1的数据的影响；
#MeanDecreaseAccuracy：表示变量替换后准确率的下降；
#MeanDecreaseGini：表示变量替换后GINI系数的降低。数值越大表示变量越重要。

#对测试集进行预测
pre_ran <- predict(wine_randomforest,newdata=test_data)
#将真实值和预测值整合到一起
obs_p_ran = data.frame(prob=pre_ran,obs=test_data$等级)
#输出混淆矩阵
table(test_data$等级,pre_ran,dnn=c("真实值","预测值"))
#绘制ROC曲线
ran_roc <- roc(test_data$等级,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线,mtry=3,ntree=500')

dd <- mRMR.data(data = datacollege2)
mRMR.classic(data = dd, target_indices = c(1), feature_count = 6)


#但是mRMR.data需要所有的数据都是numeric，所以要将里面不是numeric的转换成numeric
for (m in which(sapply(mrmr_feature, class)!="numeric")){mrmr_feature[,m]=as.numeric(mrmr_feature[,m])