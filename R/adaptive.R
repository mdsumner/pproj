library(maptools)
library(rgdal)
library(geosphere)
prj <- "+proj=sinu +lon_0=40"
data(wrld_simpl)
d <- disaggregate(subset(wrld_simpl, NAME == "Antarctica"))[1, ]

xy <- coordinates(as(as(d, "SpatialLines"), "SpatialPoints"))

pxy <- project(xy, prj)

plot(pxy)

gcbisect <- function(x1, x2) {
  xy <- project(rbind(x1, x2), prj, inv = TRUE)
  ## need to cull redundant points
  if (isTRUE(all.equal(x1, x2))) return(xy[1L,,drop = FALSE])
  gcIntermediate(xy[1L, ], xy[2L, ], n = 1)
}

dst <- numeric(nrow(pxy) - 1)
midpts <- (pxy[-nrow(pxy), ] + pxy[-1L, ])/2
for (i in seq_len(nrow(pxy) - 1)) {
  p1 <- gcbisect(pxy[i, ], pxy[i + 1L, ])
  p2 <- project(p1, prj)
  dst[i] <- sqrt((midpts[i,1] - p2[1])^2 + (midpts[i,2] - p2[2])^2)
  ##  points(p, pch = 16L, col = "red", cex = 0.7)
}

