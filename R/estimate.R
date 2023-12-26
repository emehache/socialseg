#' Estimate ...
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return A data.table
#' @export

estimate <- function(input, vars, lx = 100, ly, Ngrid, smoothing = "none", sigma, graficar = F, verbose = T) {

  if (smoothing == 'none') {
    input@data <- input@data[, vars]
    est <- spseg(input)
    return(est)
    }

  if (smoothing == "equal") {
    data <- input %>%
      distribuir(lx = lx, vars = vars, verbose = verbose)
  }

  if (smoothing == "kernel") {
    data <- input %>%
      distribuir(lx = lx, vars = vars, verbose = verbose) %>%
      suavizar(sigma = sigma)
  }

  est <- data %>%
    na.omit %>%
    spseg(x = .[,c("x","y")],
          data = .[, vars, with = F],
          method = "all",
          smoothing = "none")
    # {c(D = .@d, R = .@r,H = .@h)}

  return(est)

}
