library(rgl)
xyz <- cbind(runif(680, 1, nrow(volcano)), runif(680, 1, ncol(volcano)))
xyz <- cbind(xyz, volcano[xyz])
ind <-  t(delaunayn(xyz[,1:2]))
f <- 1/5
rgl.triangles(xyz[ind, 1], xyz[ind, 2], xyz[ind, 3] * f)
