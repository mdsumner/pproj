library(maptools)
library(rgdal)
library(geosphere)
prj <- "+proj=sinu"
data(wrld_simpl)
d <- disaggregate(subset(wrld_simpl, NAME == "Antarctica"))[1, ]

xy <- coordinates(as(as(d, "SpatialLines"), "SpatialPoints"))
pxy <- project(xy, prj)

plot(pxy)


gcbisect <- function(x1, x2) {
  llxy <- project(rbind(x1, x2), prj, inv = TRUE)
  ## duplicates due to meridian cut at -180/180
  if (isTRUE(all.equal(x1, x2))) return(xy[1L,,drop = FALSE])
  gcIntermediate(llxy[1L, ], llxy[2L, ], n = 1)
}


dst <- numeric(nrow(pxy) - 1)
midpts <- (pxy[-nrow(pxy), ] + pxy[-1L, ])/2
for (i in seq_len(nrow(pxy) - 1)) {
  p1 <- gcbisect(pxy[i, ], pxy[i + 1L, ])
  p2 <- project(p1, prj)
  dst[i] <- sqrt((midpts[i,1] - p2[1])^2 + (midpts[i,2] - p2[2])^2)
  ##  points(p, pch = 16L, col = "red", cex = 0.7)
}

mindist <- 10000 ## metres
l <- list() ## of matrices

visit <- which(dst > mindist)
count <- 1
l[[count]] <- pxy[1:(visit[1]), ]
for (j in seq_along(visit)) {
  gc <- gcIntermediate(xy[visit[j], ], xy[visit[j] + 1, ], n = dst[visit[j]]%/%mindist)
  l <- c(l, list(project(gc, prj)))
  if (j < length(visit)) l <- c(l, list(pxy[(visit[j] + 1):visit[j + 1], ])) else l <- c(l, list(pxy[visit[j]:nrow(pxy), ]))
}

m <- do.call(rbind, l)
m <- m[!is.na(m[,1]), ]
