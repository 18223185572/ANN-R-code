bary<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/bary.csv", header=T, row.names = 1)
dis1 <- vegdist(bary, method = 'bray')
dis1<-as.matrix(dis1)
as.vector(dis1)[-length(dis1)]
write.table(dis1,"C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/dis2.csv",row.names=TRUE,col.names=TRUE,sep=",")
library(geosphere)
coordin<- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/disco.csv", header=T, row.names = 1)
sitedist<-distm(coordin, fun=distVincentyEllipsoid)
sitedist<-as.data.frame(sitedist)
write.table(sitedist, "C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/discovalue.csv",row.names=TRUE,col.names=TRUE,sep=",")# 写出去


as.vector(dis1)[-length(dis1)]







