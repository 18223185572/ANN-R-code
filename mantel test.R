library(ape)
q1 <- matrix(runif(36), nrow = 6)
q2 <- matrix(runif(36), nrow = 6)
diag(q1) <- diag(q2) <- 0
mantel.test(q1, q2, graph = TRUE,
+            asymmetric relationships",
+            lab = "z-statistic", ylab = "Density",
+            sub = "The vertical line shows the observed z-statistic")


data(varespec)
data(varechem)
veg.dist <- vegdist(varespec) # Bray-Curtis
env.dist <- vegdist(scale(varechem), "euclid")
mantel(veg.dist, env.dist)
mantel(veg.dist, env.dist, method="spear")

library(ape)
chem <- read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/chemphy.csv", header=T,row.names=1)
chem1<-chem[,1]
ARGs<-read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/ARGsmantel.csv", header=T,row.names=1)
MGEs<-read.csv("C:/Users/Robin.DESKTOP-0U5O684/Desktop/data/MGEspartial.csv", header=T,row.names=1)
chem1.dist <- vegdist(chem1) # Bray-Curtis
mge.dist <- vegdist(MGEs) # Bray-Curtis
ARGs.dist <- vegdist(scale(ARGs), "euclid")
mantel(chem1.dist, ARGs.dist, method="spear")
mantel.partial(chem1.dist, ARGs.dist, mge.dist,method = "pearson", permutations = 999, strata = NULL, na.rm = FALSE, parallel = getOption("mc.cores"))

