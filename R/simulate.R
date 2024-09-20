#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @param lx Size of grid (width)
#' @param ly Size of grid (height)
#' @param vars name
#' @param bbox square
#' @param crs coordinates
#' @param compute_distances ar
#' @param ... optional
#' @return aaaa

#' @keywords internal
simulate_map <- function(gridmap, vars, N = 1) {

  dd <- gridmap$grid %>%
    st_drop_geometry %>%
    as.data.table

  if (missing(vars)) vars <- setdiff(names(dd),"i")


  tol = .Machine$double.eps
  dd <- na.omit(dd) + tol
  M <- ncol(dd)
  tot <- sum(dd)
  tot_p <- rowSums(dd)
  tot_m <- colSums(dd)
  pi_m <- tot_m/tot

  gridmap <- replicate(N, {
    # la linea siguiente hace que solo funcione para variables binarias, hay que cambiar a la multnomial
    sim_var <- sapply(floor(tot_p), \(nn) rbinom(1, size = nn, prob = pi_m[1]))
    simulated_data <- cbind(sim_var, tot_p - sim_var) %>%
      as.data.table
    names(simulated_data) <- vars

    gridmap$grid <- gridmap$grid %>%
      as.data.table %>%
      .[, (vars) := simulated_data] %>%
      .[] %>%
      st_as_sf()

    return(gridmap)

  }, simplify = F)

  if (N == 1) gridmap <- gridmap[[1]]

  return(gridmap)
}


#' @export
simulated_profile <- function(gridmap, grid_gamma, vars, N = 1) {

  if (missing(vars)) vars <- setdiff(names(dd),"i")

  dd <- gridmap$grid %>%
    st_drop_geometry %>%
    as.data.table %>%
    .[, vars, with = F]
  matriz <- gridmap$distances

  tol = .Machine$double.eps
  dd <- na.omit(dd) + tol
  M <- ncol(dd)
  tot <- sum(dd)
  tot_p <- rowSums(dd)
  tot_m <- colSums(dd)
  pi_m <- tot_m/tot

  simulaciones <- replicate(N, {
    # la linea siguiente hace que solo funcione para variables binarias, hay que cambiar a la multnomial
    sim_var <- sapply(floor(tot_p), \(nn) rbinom(1, size = nn, prob = pi_m[1]))
    simulated_data <- cbind(sim_var, tot_p - sim_var)
    colnames(simulated_data) <- vars

    return(simulated_data)

  }, simplify = FALSE)

  simulaciones <- Reduce("cbind", simulaciones)



  output <- sapply(grid_gamma, function(gamma) {
    ker <- cvmgof::kernel.function.quart(matriz/gamma) %>%
      Matrix::Matrix(sparse = T)
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t

    crossprod(ker, simulaciones)
  }, simplify = "array")

  output <- sapply(1:N, \(i) {
    ind <- (1:length(vars)) + length(vars)*(i-1)
    output[,ind,]
  }, simplify = "array")

  results <- apply(output, 3:4, \(ee) estimate_index(dd, ee)$H)
  rownames(results) <- grid_gamma


  return(results)
}
