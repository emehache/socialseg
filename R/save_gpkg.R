#' Distribute the data
#'
#' This function ...
#'

#' @export
save_gpkg <- function(gridmap = gridmap, file = file, ...) {
  st_write(gridmap$grid, file, driver = "gpkg", delete_dsn = T, ...)
}
