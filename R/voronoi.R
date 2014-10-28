
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

library(geosphere)
library(rgl)
x <- randomCoordinates(23)

xyz <- llh2xyz(cbind(x, 0))

ch <- convhulln(xyz)

ts.surf <- t(ch)
rgl.triangles(xyz[ts.surf, 1], xyz[ts.surf, 2], xyz[ts.surf, 3], 
col = "aliceblue", alpha = 0.8)
for (i in 1:(8*360)) rgl.viewpoint(i/8)

