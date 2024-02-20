#' Estimate ...
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return A data.table
#' @export

estimate <-  function(input, vars, lx = 100, ly, Ngrid, smoothing = "none", sigma = "none", indices = c("D", "H"), graficar = F, verbose = T) {


  if (smoothing == "equal") {
    datos <- input %>%
      distribuir(lx = lx, vars = vars, verbose = verbose)
  }

  if (smoothing == "kernel") {
    datos <- input %>%
      distribuir(lx = lx, vars = vars, verbose = verbose) %>%
      suavizar(sigma = sigma)
  }

  if (smoothing == 'none') {
    input@data <- input@data[, vars]
    est <- spseg(input)
  } else {
    est <- datos %>%
      na.omit %>%
      spseg(x = .[,c("x","y")],
            data = .[, vars, with = F],
            method = "all",
            smoothing = "none")
  }

  out <- est@p %>%
    as.data.table(keep.rownames = "var1") %>%
    melt.data.table(id = "var1", variable.name = "var2") %>%
    .[, .(index = 'P', coef = paste(var1, var2, sep = "-"), value)] %>%
    rbind(data.table(index = c("D", "R", "H"), coef = NA, value = c(est@d, est@r, est@h)), .) %>%
    # cbind(lx, smoothing, sigma, vars = paste0(vars, collapse="+")) %>%
    .[index %chin% indices]

  return(out)

}
