library(G1DBN)
ssd <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/datatotalfeat.csv", header=T,row.names=1)
#names<-names(ssd)
step1<-DBNScorestep1(ssd,meth="ls")
step2<-DBNScorestep2(step1$1ls,data=ssd,method="ls",alphal=0.50)
edgeG<-BuildEdges(score=step2,threshold=0.05, prec=6,trargetNames=name,
                  predNames=name)
all_parents=c(edgeG[,1])
all_targets=c(edgeG[,2])
posEdgeG1=1:dim(edgeG)[1]
posEdgesG2=(dim(edgeG)[1]+1):length(all_targets)
netAll=graph.edgelist(cbind(as.character(all_parents),
                            as.character(all_targets)))
nodeCoord=layout.fruchterman.reingold(netAll)
netG1=graph.edgelist(cbind(as.character(edgeG[,1]),as.character(edgeG[,2])))
Gltoplot=delete.edgelist(netAll,E(netAll)[posEdgeG1])
plot(netG1,layout=nodeCoord,vertex.label=get.vertex.attribute(Gltoplot,name="names"))




