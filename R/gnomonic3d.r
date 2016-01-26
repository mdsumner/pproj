library(maptools)
data(wrld_simpl)

## raw coordinates from maptools
ll <- coordinates(as(as(wrld_simpl, "SpatialLines"), "SpatialPoints"))

library(rgl)

## use the PROJ.4 sphere
a <- 6370997
##b <- 6370997
## function to produce xyz from longitude, latitude, height
## (spherical)
llh2xyz <- function(lonlatheight, rad = 6370997, exag = 1) {
  d2r <- pi / 180.0
  cosLat = cos(lonlatheight[,2] * d2r)
  sinLat = sin(lonlatheight[,2] * d2r)
  cosLon = cos(lonlatheight[,1] * d2r)
  sinLon = sin(lonlatheight[,1] * d2r)
  x = rad * cosLat * sinLon
  y = rad * cosLat * cosLon
  z = (lonlatheight[,3] * exag) + rad * sinLat
  cbind(x, y,-z)
}


xyz <- llh2xyz(cbind(ll, 0), rad = a)

## project 
library(rgdal)
prj <- "+proj=gnom +lon_0=0 +lat_0=-90 +ellps=sphere"
pxy <- project(ll, prj)

## these are the projected map points on the plane tangent
## to the south pole
pxyz <- cbind(pxy, a)

## plotting viewpoint for rgl
um <- structure(c(-0.765494763851166, 0.532267570495605, 0.361521005630493, 
                  0, -0.59319019317627, -0.366132825613022, -0.716980457305908, 
                  0, -0.249262124300003, -0.763299465179443, 0.596011281013489, 
                  0, 0, 0, 0, 1), .Dim = c(4L, 4L))

## reduce the input map the south
maxlat <- -10
llsub <- ll[,2] < maxlat

## plot 
bg3d(bg = "black")
plot3d(xyz, col = "dodgerblue", axes = FALSE)
points3d(pxyz[llsub, ], col = "#6AB787FF")
rgl.viewpoint(userMatrix = um, zoom = 0.5)

## rays from the projection point
ptz2 <- cbind(project(ll[llsub, ][sample(sum(llsub), 100), ], prj), a)
for (i in seq_len(nrow(ptz2))) {
  lines3d(rbind(c(0, 0, 0), ptz2[i,,drop = FALSE]), color = "grey", lwd =1)
}
