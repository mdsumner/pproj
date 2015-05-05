library(raster)
r <- raster(volcano)
xy <- cbind(runif(10, xmin(r), xmax(r)), runif(10, ymin(r), ymax(r)))
cn <- extract(r, xy, cellnumbers = TRUE)[,"cells"]

## something like this for fast bilinear
d1 <- spDistsN1(xyFromCell(r, cn[1]), xy[1,,drop = FALSE])
d2 <- spDistsN1(xyFromCell(r, adjacent(r, cellFromXY(r, xy[1,,drop = FALSE]), pairs = FALSE)), xy[1,,drop = FALSE])

weighted.mean(extract(r, c(cn[1], adjacent(r, cellFromXY(r, xy[1,,drop = FALSE]), pairs = FALSE))),  c(d1, d2)/(sum(c(d1, d2))))
extract(r, xy[1,,drop = FALSE], method = "bilinear")
