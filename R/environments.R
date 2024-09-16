#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return gridmap
#' @export
#'
environments <- function(gridmap, gamma) {

  vars <- setdiff(names(input), "geom", "id", "i")

  grid <- gridmap$grid
  original <- st_drop_geometry(grid)
  envir <- gridmap$grid
  matriz <- gridmap$distances

  if (nucleo == "quartic") {
    ker <- cvmgof::kernel.function.quart(matriz/sigma) %>%
      Matrix::Matrix(sparse = T)
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
  }

  if (nucleo == "uniforme") {
    ker <- matriz<=sigma
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
  }

  smoothed <- copy(original) %>%
    .[, (vars) := lapply(.SD, \(x) crossprod(ker, x)), .SDcols = vars] %>%
    .[]

  envir <- envir %>%
    as.data.table %>%
    .[, (vars) := smoothed[,vars, with = F]] %>%
    .[] %>%
    st_as_sf()





  tol <- .Machine$double.eps

  dd <- grid %>%
    as.data.table %>%
    .[, vars, with = F]

  ee <- smoothed[, vars, with = F]

  dd <- dd + tol
  ee <- ee + tol
  dd <- na.omit(dd)
  ee <- na.omit(ee)
  M <- ncol(dd)
  tot <- sum(dd)
  tot_p <- rowSums(dd)
  tot_m <- colSums(dd)
  pi_m <- tot_m/tot
  pi_pm <- ee/rowSums(ee)
  ent_p <- rowSums(-(dd/rowSums(dd))*log((dd/rowSums(dd)), base = M))
  Ep <- rowSums(-pi_pm*log(pi_pm, base = M))
  E <- -sum(pi_m * log(pi_m, base = M))
  H <- 1 - (sum(tot_p * Ep) / (tot * E))

  values <- list(H = H, tot = tot, E = E, tot_p = tot_p, ent_p = ent_p)

  output <- list(grid = grid, environments = envir, input = input, distances = distances, values = values)
  class(output) <- "environmentmap"

  return(output)

}
