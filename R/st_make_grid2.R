#' Create a regular tesselation over the bounding box of an sf or sfc object
#'
#' Create a square or hexagonal grid covering the bounding box of the geometry of an sf or sfc object
#' @param x object of class \link{sf} or \link{sfc}
#' @param cellsize numeric of length 1 or 2 with target cellsize: for square or rectangular cells the width and height, for hexagonal cells the distance between opposite edges (edge length is cellsize/sqrt(3)). A length units object can be passed, or an area unit object with area size of the square or hexagonal cell.
#' @param offset numeric of length 2; lower left corner coordinates (x, y) of the grid
#' @param n integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @param crs object of class \code{crs}; coordinate reference system of the target grid in case argument \code{x} is missing, if \code{x} is not missing, its crs is inherited.
#' @param what character; one of: \code{"polygons"}, \code{"corners"}, or \code{"centers"}
#' @param square logical; if \code{FALSE}, create hexagonal grid
#' @param flat_topped logical; if \code{TRUE} generate flat topped hexagons, else generate pointy topped
#' @return Object of class \code{sfc} (simple feature geometry list column) with, depending on \code{what} and \code{square},
#' square or hexagonal polygons, corner points of these polygons, or center points of these polygons.
#' @examples
#' plot(st_make_grid(what = "centers"), axes = TRUE)
#' plot(st_make_grid(what = "corners"), add = TRUE, col = 'green', pch=3)
#' sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
#' plot(st_make_grid(sfc, cellsize = .1, square = FALSE))
#' plot(sfc, add = TRUE)
#' # non-default offset:
#' plot(st_make_grid(sfc, cellsize = .1, square = FALSE, offset = c(0, .05 / (sqrt(3)/2))))
#' plot(sfc, add = TRUE)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' g = st_make_grid(nc)
#' plot(g)
#' plot(st_geometry(nc), add = TRUE)
#' # g[nc] selects cells that intersect with nc:
#' plot(g[nc], col = '#ff000088', add = TRUE)
#' @keywords internal
st_make_grid2 = function(x,
                         bb,
                        cellsize = c(diff(st_bbox(x)[c(1,3)]), diff(st_bbox(x)[c(2,4)]))/n,
                        # offset = st_bbox(x)[c("xmin", "ymin")], n = c(10, 10),
                        crs = if (missing(x)) NA_crs_ else st_crs(x),
                        what = "polygons", square = TRUE, flat_topped = FALSE) {

  offset <- bb[c("xmin", "ymin")]

  # if (!inherits(crs, "crs"))
  #   crs = st_crs(crs) # #2057
  # if (missing(x) && missing(cellsize) && missing(offset)
  #     && missing(n) && missing(crs)) # create global 10 x 10 degree grid
  #   return(st_make_grid(cellsize = c(10,10), offset = c(-180,-90), n = c(36,18),
  #                       crs = st_crs(4326), what = what))

  # if (! square) { # hexagons:
  #   if (!is.null(crs$ud_unit)) {
  #     if (inherits(cellsize, "units")) {
  #       if (units::ud_are_convertible(units(cellsize), "m^2")) { # size in area
  #         # convert: https://github.com/r-spatial/sf/issues/1505
  #         a = sqrt(cellsize * 2 / (3 * sqrt(3)))
  #         cellsize = a * sqrt(3)
  #       }
  #       units(cellsize) = units(crs$ud_unit)
  #       cellsize = units::drop_units(cellsize)
  #     }
  #     if (inherits(offset, "units")) {
  #       units(offset) = units(crs$ud_unit)
  #       offset = units::drop_units(offset)
  #     }
  #   }
  #   hex = make_hex_grid(x, dx = cellsize[1]/sqrt(3), pt = offset, what = what,
  #                       flat_topped = flat_topped)
  #   if (what == "corners")
  #     hex = st_cast(hex, "POINT")[x]
  #   return(hex)
  # }

  # bb = if (!missing(n) && !missing(offset) && !missing(cellsize)) {
  #   cellsize = rep(cellsize, length.out = 2)
  #   n = rep(n, length.out = 2)
  #   bb_wrap(c(offset, offset + n * cellsize))
  # } else
  #   st_bbox(x)

  cellsize_missing = if (! missing(cellsize)) {
    cellsize = rep(cellsize, length.out = 2)
    FALSE
  } else
    TRUE

  if (!is.null(crs$ud_unit)) {
    if (inherits(cellsize, "units")) {
      if (units::ud_are_convertible(units(cellsize), "m^2")) # size in area
        cellsize = sqrt(cellsize)
      units(cellsize) = units(crs$ud_unit)
      cellsize = units::drop_units(cellsize)
    }
    if (inherits(offset, "units")) {
      units(offset) = units(crs$ud_unit)
      offset = units::drop_units(offset)
    }
  }

  # if (missing(n)) {
    nx = ceiling((bb[3] - bb[1])/cellsize[1])
    ny = ceiling((bb[4] - bb[2])/cellsize[2])
  # } else {
  #   n = rep(n, length.out = 2)
  #   nx = n[1]
  #   ny = n[2]
  # }

  # corner points:
  if (cellsize_missing) {
    xc = seq(offset[1], bb[3], length.out = nx + 1)
    yc = seq(offset[2], bb[4], length.out = ny + 1)
  } else {
    xc = offset[1] + (0:nx) * cellsize[1]
    yc = offset[2] + (0:ny) * cellsize[2]
  }

  if (what == "polygons") {
    ret = vector("list", nx * ny)
    square = function(x1, y1, x2, y2)
      st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
    for (i in 1:nx)
      for (j in 1:ny)
        ret[[(j - 1) * nx + i]] = square(xc[i], yc[j], xc[i+1], yc[j+1])
    st_sfc(ret, crs = crs)
  } else if (what == "centers") {
    e = expand.grid(x = xc[-1] - 0.5 * diff(xc[1:2]),
                    y = yc[-1] - 0.5 * diff(yc[1:2]),
                    KEEP.OUT.ATTRS = FALSE)
    st_geometry(st_as_sf(e, coords = c("x", "y"), crs = crs))
  } else if (what == "corners") {
    e = expand.grid(x = xc, y = yc, KEEP.OUT.ATTRS = FALSE)
    st_geometry(st_as_sf(e, coords = c("x", "y"), crs = crs))
  } else
    stop("unknown value of `what'")
}

### hex grid tesselation that
## - covers a bounding box st_bbox(obj)
## - contains pt
## - has x spacing dx: the shortest distance between x coordinates with identical y coordinate
make_hex_grid = function(obj, pt, dx, what, flat_topped = TRUE) {

  dy = sqrt(3) * dx / 2
  bb = st_bbox(obj)
  if (!flat_topped) { # pointy topped -- swap x and y:
    ylim = bb[c("xmin", "xmax")]
    xlim = bb[c("ymin", "ymax")]
    pt = pt[2:1]
  } else {
    xlim = bb[c("xmin", "xmax")]
    ylim = bb[c("ymin", "ymax")]
  }
  offset = c(x = (pt[1] - xlim[1]) %% dx, y = (pt[2] - ylim[1]) %% (2 * dy))
  x0 = seq(xlim[1] - dx, xlim[2] + 2 * dx, dx) + offset[1]
  y0 = seq(ylim[1] - 2 * dy, ylim[2] + 2 * dy, dy) + offset[2]

  y  <- rep(y0, each = length(x0))
  x  <- rep(c(x0, x0 + dx / 2), length.out = length(y))
  xy = cbind(x, y) # the coordinates

  # compute the indexes, using double coordinates:
  odd  <- seq(1, by = 2, length.out = length(x0))
  even <- seq(2, by = 2, length.out = length(x0))
  xi <- rep(c(odd, even), length.out = length(y))
  yi <- rep(seq_along(y0), each = length(x0))
  # hexagon centers are columns with x index 3, 6, 9, ... :
  centers = cbind(xi,yi)[xi %in% seq(3, max(xi) - 2, by = 3) & yi > 1 & yi < max(yi),]

  # relative offset in double coordinates, https://www.redblobgames.com/grids/hexagons/
  nx = length(x0)
  xy_pattern = rbind(c(-2,0), c(-1,-1), c(1,-1), c(2,0), c(1,1), c(-1,1), c(-2,0))
  i_from_x = function(x) ((x[,1] - 1) %/% 2 + 1) + (x[,2] - 1) * nx

  mk_point = if (flat_topped)
    function(center) st_point(xy[i_from_x(matrix(center, ncol = 2)),])
  else
    function(center) st_point(xy[i_from_x(matrix(center, ncol = 2)),2:1])

  mk_pol = if (flat_topped)
    function(center) {
      m = matrix(center, ncol=2, nrow = 7, byrow=TRUE) + xy_pattern
      st_polygon(list(xy[i_from_x(m),]))
    }
  else
    function(center) {
      m = matrix(center, ncol=2, nrow = 7, byrow=TRUE) + xy_pattern
      st_polygon(list(xy[i_from_x(m),2:1]))
    }

  if (what == "centers")
    st_sfc(lapply(seq_len(nrow(centers)), function(i) mk_point(centers[i,])), crs = st_crs(bb))
  else # points:
    st_sfc(lapply(seq_len(nrow(centers)), function(i) mk_pol(centers[i,])), crs = st_crs(bb))
}
