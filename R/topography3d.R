library(marmap)
library(geosphere)
library(raster)
library(geometry)
## one part of this is upside down
llh2xyz <- function(lonlatheight, rad = 500) {
cosLat = cos(lonlatheight[,2] * pi / 180.0)
sinLat = sin(lonlatheight[,2] * pi / 180.0)
cosLon = cos(lonlatheight[,1] * pi / 180.0)
sinLon = sin(lonlatheight[,1] * pi / 180.0)

x = rad * cosLat * cosLon
y = rad * cosLat * sinLon
z = lonlatheight[,3] + rad * sinLat

cbind(x, y, z)
}

 b <- getNOAA.bathy(-180, 180, -90, -30, resolution = 60, keep=TRUE, antimeridian=FALSE)
r <- raster(list(x = seq(-180, 180, length = nrow(b)), y = seq(-90, -30, length = ncol(b)), z = b))

xyz <- randomCoordinates(7600)
xyz <- xyz[xyz[,2] <= -30, ]

xyz <- cbind(xyz, extract(r, xyz[,1:2]))
#xyz[,1:2] <- project(xyz[,1:2], "+proj=laea +lat_0=-90")
f <- 50
xyz <- llh2xyz(xyz, rad = 63780)
ind <-  t(delaunayn(xyz[,1:2]))

rgl.triangles(xyz[ind, 1], xyz[ind, 2], xyz[ind, 3] )
