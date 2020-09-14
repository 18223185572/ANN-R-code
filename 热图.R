library(pheatmap)
core3 <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/coreARGs.csv", header=T,row.names = 1)
windowsFonts(TNM = windowsFont("Times New Roman"))
heatmap(core3, fontsize=10)

pheatmap(core3, 
         cluster_cols=FALSE,
         cluster_rows=FALSE, 
         color = colorRampPalette(colors = c( "#F8766D","white","#00BFC4"))(100),
         fontsize_row=28,
         fontsize_col=0.1)





pheatmap(core2, ann_colors = list(
  Time = c("white", "#F8766D"),
  CellType = c(CT1 = "#F8766D", CT2 = "#00BFC4"),
  GeneClass = c(Path1 = "#F8766D", Path2 = "#E7298A", Path3 = "#00BFC4")
)
)
