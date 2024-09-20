#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return gridmap
#' @export
#'
environments <- function(gridmap, gamma, vars, nucleo = "quartic") {

  grid <- gridmap$grid
  original <- st_drop_geometry(grid)
  envir <- gridmap$grid
  matriz <- gridmap$distances

  if (missing(vars)) vars <- setdiff(names(grid), c("geom", "geometry", "id", "i"))

  if (nucleo == "quartic") {
    ker <- cvmgof::kernel.function.quart(matriz/gamma) %>%
      Matrix::Matrix(sparse = T)
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
  }

  if (nucleo == "uniforme") {
    ker <- matriz<=gamma
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

  dd <- grid %>%
    as.data.table %>%
    .[, vars, with = F]

  ee <- smoothed[, vars, with = F]

  values <- estimate_index(dd, ee)

  envir <- cbind(envir, Ep = values$Ep, tot_p = values$tot_p, Hp = values$Hp)

  output <- list(grid = envir, input = input, distances = matriz, values = values, lx_ly = gridmap$lx_ly, sigma = gridmap$sigma, gamma = gamma)
  class(output) <- "gridmap"

  return(output)

}


#' @keywords internal
estimate_index <- function(dd, ee, tol = .Machine$double.eps) {

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
  # H <- 1 - (sum(tot_p * Ep) / (tot   * E))
  Hp <- tot_p*(E-Ep)/tot/E
  H <- sum(Hp)

  values <- list(H = H, Hp = Hp, tot = tot, E = E, tot_p = tot_p, ent_p = ent_p, Ep = Ep)
  return(values)
}
