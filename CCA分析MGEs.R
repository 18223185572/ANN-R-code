library(ggrepel)
library(vegan)
mgearg <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/mgearg.csv", header=T,row.names=1)
mge<-mgearg[,1:10]
arg<-mgearg[,11:17]
#对第一、二组指标进行中心化和标准化，用于消除数据数量级的影响
mgez<-scale(mge)+10  #（输出序号1）
argz<-scale(arg)+10  #（输出序号2）
cca_analysis1<-cca(argz,mgez)
result1<-summary(cca_analysis1)

sp=as.data.frame(result1$species[,1:2]*30)###提取相应变量坐标，乘以5是使图美观，不影响分析
st=as.data.frame(result1$sites[,1:2],row.names = F)###提取样方坐标,如果不想让样点名称显示可以st=as.data.frame(result$sites[,1:2],row.names = F)
yz=as.data.frame(result1$biplot[,1:2])###提取解释变量坐标
group=as.data.frame(c(rep("I",10),rep("II",10),rep("III",10)))####创建分组信息
colnames(group)="groups"####将分组列命名为groups
ggplot() +
  geom_segment(data = sp,aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.8,colour = "#F8766D")+  
  geom_text_repel(data = sp,size=7,aes(CCA1,CCA2,label=row.names(sp)))+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = CCA1, yend = CCA2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.8,colour = "#00BFC4")+
  geom_text_repel(data = yz,size=7,aes(CCA1,CCA2,label=row.names(yz)))+
    labs(x="CCA1 58.76%",y="CCA2 3.22%")+ ##RDA1，RDA2的值需要从结果中获得
  geom_hline(yintercept=0,linetype=3,size=1)+ 
  geom_vline(xintercept=0,linetype=3,size=1)+
  theme(legend.position="none", #不需要图例
        axis.text.x=element_text(colour="black",size=20), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(size=20,face="plain"), #设置x轴刻度标签的字体属性
        axis.title.y=element_text(size = 20,face="plain"), #设置y轴的标题的字体属性
        axis.title.x=element_text(size = 20,face="plain")) #设置x轴的标题的字体属性
