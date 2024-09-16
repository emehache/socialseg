#' Distribute the data
#'
#' This function ...
#'

#' @export
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x + 1, base)
  inv <- function(x) base^(-x) - 1
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv)
}

#' @export
log_trans <- function(base = exp(1)) {
  trans <- function(x) log(x + .03, base)
  inv <- function(x) base^(x) - .03
  scales::trans_new(paste0("log-", format(base)), trans, inv)
}


#' @export
plot.gridmap <- function(gridmap, var, poligono = T, contour = F, per = .9, ...) {

  graf <- ggplot() +
    geom_sf(data = gridmap$grid, aes(fill = .data[[var]]), col = NA) +
    theme_bw()

  graf <- graf +
    # scale_fill_gradient(low = "#FEF001", high = "#F00505",
    #                     na.value = "transparent", trans = log_trans(base = 2), ...)
    scale_fill_gradient2(low = "blue", high = "red", mid = "gray95", midpoint = 0,
                        na.value = "transparent", ...)


  if (poligono) {
    graf <- graf +
      geom_sf(data = gridmap$input, linewidth = .03, alpha = .3, col = 1, fill = NA)
  }

  return(graf)
}

# #' @export
# plot.environmentmap <- function(gridmap, ...) {
#
#   gridmap$grid <- gridmap$environments
#   # grid$environments <- NULL
#   class(gridmap) <- "gridmap"
#   plot(gridmap, ...)
#
# }

#' @export
coef.gridmap <- function(gridmap) {

out <- c(H = gridmap$values$H)
if ("gamma" %in% names(gridmap)) print(paste0("Entornos construidos con gamma = ", gridmap[["gamma"]]))

return(out)
}
