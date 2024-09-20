#' Distribute the data
#'
#'
#'
#' @param input Path to the input file
#' @param lx Size of
#' @return gridmap
#' @export

diff_grid <- function(gridmapX, gridmapY, var) {

  gridx <- gridmapX$grid
  gridy <- gridmapY$grid

  bb <- c(apply(rbind(st_bbox(gridx), st_bbox(gridy))[,1:2], 2, min),
          apply(rbind(st_bbox(gridx), st_bbox(gridy))[,3:4], 2, max)) %>%
    st_bbox(crs = st_crs(gridx))

  vars <- paste0(var, c(".x", ".y"))

  ### Avisar que gridmapX y gridmapY deben tener el mismo lx_ly
  grid <- st_make_grid(bb, cellsize = gridmapX$lx_ly)

  ies <- union(gridx$i, gridy$i)
  grid <- grid %>%
    as.data.table %>%
    .[, i:= .I] %>%
    .[i %in% ies] %>%
    merge(st_drop_geometry(gridx), all = T) %>%
    merge(st_drop_geometry(gridy), all = T) %>%
    .[, (setdiff(names(.), "geometry")) := lapply(.SD, \(x) nafill(x, fill = 0)), .SDcols = setdiff(names(.), "geometry")] %>%
    .[]

  var1 <- grid[, vars[1], with = F]
  var2 <- grid[, vars[2], with = F]

  grid <- grid %>%
    .[, (var) := var1-var2] %>%
    .[, c("i", var, "geometry"), with = F] %>%
    st_as_sf()

  gridmapX$grid <- grid

  return(gridmapX)
}
