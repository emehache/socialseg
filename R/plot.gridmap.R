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

  if (inherits(gridmap, "sf")) gridmap <- list(grid = gridmap, input = gridmap)

  if (missing(var)) {
    vars <- setdiff(names(gridmap$grid), c("geom", "geometry", "id", "i"))
    var <- vars[1]
    if (length(vars) > 2) print(sprintf("Plotting %s. There are %s other variables available to plot.", var, length(vars) - 1))
    else print(sprintf("Plotting %s. There is another variable available to plot.", var))
    }

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

  if (contour) {

    q <- gridmap$grid[, var, with = F] %>%
      as.data.table %>%
      .[order(-get(var))] %>%
      .[ , prop := cumsum(get(var))/sum(get(var))] %>%
      .[prop >= 1-per, get(var)[1]]

    data2 <- gridmap$grid[, var, with = F] %>%
      as.data.table %>%
      .[get(var) >= q] %>%
      .[] %>%
      st_as_sf()

    graf <- graf +
      geom_sf(data = data2, fill = 4, alpha = .6, col = NA)

  }

  return(graf)
}


#' @export
coef.gridmap <- function(gridmap) {

out <- c(H = gridmap$values$H)
if ("gamma" %in% names(gridmap)) names(out) <- sprintf("H(%s)", gridmap$gamma)

return(out)
}



#' @export
print.gridmap <- function(gridmap) {

  print(head(gridmap$grid,1))
  cat("--------- \n")
  print(head(gridmap$input, 0))
  cat("--------- \n")
  print(str(gridmap[!names(gridmap) %in% c("grid","input")]))
  cat("--------- \n")
  invisible(cat(sprintf("Object of class %s", class(gridmap))))

  }
