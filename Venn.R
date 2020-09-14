library(VennDiagram)
grid.newpage(); #清空画板，开始画新图
venn.plot1 <- draw.pairwise.venn(602, 547, 499,#区域数字
                                 c("Wet season", "Dry season"),#区域名称
                                 cex = 2.5, #字号大小
                                 cat.cex = 2.5,#字号大小
                                 fill = c("#F8766D", "#00BFC4"),#填充颜色
                                 scaled = FALSE, #大小不随着数字变化
                                cat.dist = -0.09 );#区域名称到边框的距离
grid.draw(venn.plot1)#显示深色图形

