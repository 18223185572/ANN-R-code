library(e1071)
library(randomForest)
library(dplyr)
library(ggplot2)
sourcetrack <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/sourcetrial.csv", header=T,row.names=1)
random<-randomForest(ARGs~.,data=sourcetrack,importance=TRUE, ntree=500)
print(random)
#自变量的重要程度
importance(random)
#自变量重要性排序图
varImpPlot(random)
incMSE<-as.data.frame(importance(random)[,1])
options(digits=2)
Variables <-rownames(incMSE)
inc<-cbind(Variables,incMSE)
colnames(inc)<-c("Variables","IncMSE")
rownames(inc)<-c("g_Ruminococcus","g_Enterococcus","g_Lachnospiraceae","g_Arcobacter","g_Enterobacteriaceae",
                 "g_Flavobacterium","g_Rhodospirillaceae","g_Pseudoalteromonas","g_Megamonas","g_Hydrogenophaga",
                 "g_Limnohabitans","g_Delftia","g_Methylobacterium","g_Dechloromonas","g_Methylibium",
                 "g_Enhydrobacter","g_Pleomorphomonas")
group<-as.matrix(c("Soil","Soil","Soil","Water","Water","Water","Water","Fecal","Fecal","Fecal","Fecal","Fecal",
                       "WWTPs","WWTPs","WWTPs","WWTPs","WWTPs"))
colnames(group)<-c("Group")
inc1<-cbind(inc,group)
inc2<-inc1[,-1]
Variables <-as.matrix(rownames(inc2))
inc3<-cbind(Variables,inc2)
colnames(inc3)<-c("Variables","IncMSE","Group")
write.csv(inc3,"C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/inc3.csv")
inc4<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/inc3.csv", header=T)
#原始数据筛选（category,term，pval)散列，按照category，-log10(pval)排序
data<-inc4%>%select(Variables,Group,IncMSE)%>%arrange(Group,desc(IncMSE))
#画图时改变geom_bar的自动排序
data$Variables<-factor(data$Variables,levels = unique(data$Variables),ordered = T)
windowsFonts(TNM = windowsFont("Times New Roman"))
ggplot(data)+
  geom_bar(aes(x=IncMSE,y=Variables,fill=Group),colour = "white",stat = 'identity')+
  coord_flip()+
  theme(legend.position=c(0.965,0.89), #图例在右上方
        legend.title=element_text(size=0.001),
        legend.text=element_text(size=20),#图例尺寸
        axis.text.x=element_text(colour="black",size=20,face="italic",angle = 90), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="italic",angle = 0), #设置x轴刻度标签的字体属性
        axis.title.x=element_text(size = 20,face="plain"), #设置x轴的标题的字体属性
        axis.title.y=element_text(size = 20,face="plain"), #设置y轴的标题的字体属性
        plot.title = element_text(size=20,face="bold",hjust = 0.5))+ #设置总标题的字体属性
  ylab("Biomaker")+xlab("%IncMSE") #设置x轴和y轴的标题








