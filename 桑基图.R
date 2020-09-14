library(networkD3)
a = read.table('C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/sankey.csv', header=TRUE, sep=',')# 读取数据

# 导入数据的起点终点是factor类型，需要将其转为chr，否则之后unique的是因子水平，不能进行合并。
a$s = as.character(a$s)    
a$t = as.character(a$t)  

Sankeylinks<-a  #取边的数据
#取点的数据，用unique去重，转化为数据框格式，并将列名设置为“name”
Sankeynodes<-data.frame(name=unique(c(Sankeylinks$s,Sankeylinks$t)))   
#增加设置1列index，方便后面合并，取值为0到总行数-1
Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)  
#将边数据与点数据合并，来源点即s为第4列
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="s",by.y="name") 
#将边数据与点数据合并，目标点即t为第5列
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="t",by.y="name")  


Sankeydata<-Sankeylinks[,c(4,5,3)]; #取第4、5、3列数据，及来源、目标、边的值或权重
names(Sankeydata)<-c("Source","Target","Value")  #将三列数据分别命名
Sankeyname<-Sankeynodes[,1,drop=FALSE]  #取点的名称，即第一列

sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",  
              Target = "Target", Value = "Value", NodeID = "name",  
              fontSize = 8, nodeWidth = 20)  









