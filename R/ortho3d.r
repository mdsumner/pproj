#' Illustrate orthographic
#'
#' May need 'rgl::rglwidget()' if you don't have interactive graphics.
#'
#' @return nothing, side effect a 3D plot
#' @export
#'
#' @examples
#' do_ortho()
do_ortho <- function() {

  ll <- get_map(na.rm = TRUE)

## use the PROJ.4 sphere
a <- 6370997



xyz <- llh2xyz(cbind(ll, 0), rad = a)


prj <- "+proj=ortho +lon_0=0 +lat_0=-90 +ellps=sphere"
tf <- tempfile()
sink(tf)
on.exit(unlink(tf), add = TRUE)
pxy <- reproj::reproj_xy(ll, prj, source = "OGC:CRS84")
sink(NULL)

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
rgl::bg3d(bg = "black")
rgl::plot3d(xyz, col = "dodgerblue", axes = FALSE)
rgl::points3d(pxyz[llsub, ], col = "#6AB787FF")
rgl::view3d(userMatrix = um, zoom = 0.5)

## rays from the projection point
ptz2 <- cbind(reproj::reproj_xy(ll[llsub, ][sample(sum(llsub), 100), ], prj, source = "OGC:CRS84"), a)
for (i in seq_len(nrow(ptz2))) {
  rgl::lines3d(rbind(c(ptz2[i, 1:2], 0), ptz2[i,,drop = FALSE]), color = "grey", lwd =1)
}
}
