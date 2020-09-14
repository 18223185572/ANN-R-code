library(Hmisc)
data <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/argmgecor.csv", header=T,row.names = 1)
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat){
ut <- upper.tri(cormat) 
data.frame( row = rownames(cormat)[row(cormat)[ut]],
column = rownames(cormat)[col(cormat)[ut]], cor =(cormat)[ut], p = pmat[ut])}
#举个栗子
res3 <- rcorr(as.matrix(data))
corr<-flattenCorrMatrix(res3$r, res3$P)
write.csv(corr,file = 'C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/mgeargcorrnet.csv')



