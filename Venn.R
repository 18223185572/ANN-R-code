
library(VennDiagram)
venn.plot <- draw.pairwise.venn(100, 70, 30, c("First", "Second"));
grid.draw(venn.plot);
grid.newpage();
grid.newpage(); #清空画板，开始画新图
venn.plot <- draw.pairwise.venn(
  area1 = 177,  #区域1的数
  area2 = 214,   #区域2的数
  cross.area = 120,  #交叉数
  category = c("Wet season", "Dry season"),#分类名称
  fill = c("#F8766D", "#619CFF"),#区域填充颜色
  lty = "blank",  #区域边框线类型
  cex = 2,        #区域内部数字的字体大小
  cat.cex = 2,    #分类名称字体大小
  cat.pos = c(165, 180), #分类名称在圆的位置，默认正上方，通过角度进行调整
  cat.dist = 0.04,   #分类名称距离边的距离（可以为负数）
  cat.just = list(c(-1, -1), c(1, 1)),  #分类名称的位置
  ext.pos = 30,  #线的角度 默认是正上方12点位置
  ext.dist = -0.05,   #外部线的距离
  ext.length = 0.85,  #外部线长度
  ext.line.lwd = 2,  #外部线的宽度
  ext.line.lty = "dashed"   #外部线为虚线
);
grid.draw(venn.plot); #显示图形

