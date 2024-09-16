#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return gridmap
#' @export
seg_profile <- function(gridmap, grid_gamma, vars, nucleo = "quartic") {


  grid <- gridmap$grid
  original <- st_drop_geometry(grid)
  envir <- gridmap$grid
  matriz <- gridmap$distances

  if (missing(vars)) vars <- setdiff(names(grid), c("geom", "geometry", "id", "i"))

  original <- original[, vars, with = F]

  result <- sapply(grid_gamma, function(gamma) {

    if (nucleo == "quartic") {
      ker <- cvmgof::kernel.function.quart(matriz/gamma) %>%
        Matrix::Matrix(sparse = T)
      ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
    }

    if (nucleo == "uniforme") {
      ker <- matriz<=gamma
      ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
    }

    # smoothed <- copy(original) %>%
    #   .[, (vars) := lapply(.SD, \(x) crossprod(ker, x)), .SDcols = vars] %>%
    #   .[]



    tol <- .Machine$double.eps

    dd <- original
    ee <- crossprod(ker, as.matrix(copy(original)))

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
    # H <- 1 - (sum(tot_p * Ep) / (tot * E))
    Hp <- tot_p*(E-Ep)/tot/E
    H <- sum(Hp)
    return(H)
  })

  output <- data.table(gamma = grid_gamma, H = result)

  return(output)

}
