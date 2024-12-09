#' Smooth the data
#'
#' This function ...
#'
#' @param data ...
#' @param sigma ...
#' @return A data.table
#' @export
smoothgrid <- function(gridmap, sigma, vars, nucleo = "quartic") {

  grid <- gridmap$grid
  distribuido <- st_drop_geometry(grid)


  if (sigma == 0) return(distribuido)
  if (missing(vars)) vars <- setdiff(names(distribuido), c("id", "i"))
  distribuido <- na.omit(distribuido)

  # if (missing(matriz)) {
  #   matriz <- grid %>%
  #     st_geometry() %>%
  #     st_centroid() %>%
  #     st_distance(which = "Euclidean") %>%
  #     as.numeric %>%
  #     matrix(nrow(grid))
  # }

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

  suave <- copy(distribuido) %>%
    .[, (vars) := lapply(.SD, \(x) crossprod(ker, x)), .SDcols = vars] %>%
    .[]

  suavizado <- grid

  suavizado <- suavizado %>%
    as.data.table %>%
    .[, (vars) := suave[,vars, with = F]] %>%
    .[] %>%
    st_as_sf()


  tol <- .Machine$double.eps

  dd <- suavizado %>%
    as.data.table %>%
    .[, vars, with = F]

  dd <- dd + tol
  dd <- na.omit(dd)
  M <- ncol(dd)
  tot <- sum(dd)
  tot_p <- rowSums(dd)
  tot_m <- colSums(dd)
  pi_m <- tot_m/tot
  ent_p <- rowSums(-(dd/rowSums(dd))*log((dd/rowSums(dd)), base = M))
  E <- -sum(pi_m * log(pi_m, base = M))
  H <- 1 - (sum(tot_p * ent_p) / (tot * E))


  values <- list(H = H, tot = tot, E = E, tot_p = tot_p, ent_p = ent_p)

  output <- gridmap
  output$grid <- suavizado
  output$values <- values
  output$sigma <- sigma
  # output <- list(grid = suavizado, input = gridmap$input, distances = gridmap$distances, values = values, lx_ly = gridmap$lx_ly, sigma = sigma)
  # class(output) <- "gridmap"

  return(output)

}
