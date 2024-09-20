#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return gridmap

# #' @export
# seg_profile <- function(gridmap, grid_gamma, vars, nucleo = "quartic") {
#   grid <- gridmap$grid
#   dd <- st_drop_geometry(grid)
#   matriz <- gridmap$distances
#
#   if (missing(vars)) vars <- setdiff(names(grid), c("geom", "geometry", "id", "i"))
#
#   dd <- dd[, vars, with = F]
#
#   output <- sapply(grid_gamma, function(gamma) {
#
#     if (nucleo == "quartic") {
#       ker <- cvmgof::kernel.function.quart(matriz/gamma) %>%
#         Matrix::Matrix(sparse = T)
#       ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
#     }
#
#     if (nucleo == "uniforme") {
#       ker <- matriz<=gamma
#       ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
#     }
#
#     tol <- .Machine$double.eps
#
#     ee <- crossprod(ker, as.matrix(dd))
#
#     H <- estimate_index(dd, ee)$H
#     return(H)
#   })
#
#   # output <- data.table(gamma = grid_gamma, H = result)
#   names(output) <- grid_gamma
#   class(output) <- "seg_profile"
#
#   return(output)
# }

#' @export
plot.seg_profile <- function(seg_profile){

    ggplot(seg_profile$results) +
      geom_hline(yintercept = 0, col = "gray") +
      geom_ribbon(aes(x = seg_profile$grid_gamma, ymin = `.01`, ymax = `.99`, fill = "band"),
                  alpha = .75, show.legend = F) +
      geom_line(aes(x = seg_profile$grid_gamma, y = H)) +
      annotate(geom = "point",
               x = c(seg_profile$values["gamma_micro"], seg_profile$values["gamma_macro"]),
               y = c(seg_profile$values["Hmicro"], seg_profile$values["Hmacro"]), col = 4) +
      annotate(geom = "segment",
               x = c(seg_profile$values["gamma_micro"], seg_profile$values["gamma_macro"]),
               y = c(0, 0),
               yend = c(seg_profile$values["Hmicro"], seg_profile$values["Hmacro"]),
               lty = 2, col = 4) +
      xlab(expression(gamma)) +
      theme_bw()

}


#' @export
seg_profile <- function(gridmap, vars, frac = .25, L = 5, grid_gamma, N = 100, nucleo = "quartic") {

  if (N == 1) stop("N must be greater than 1, otherwise set N=0.")

  grid <- gridmap$grid
  matriz <- gridmap$distances
  sigma <- gridmap$sigma

  bb <- st_bbox(grid)
  diagonal <- norm(c(bb[3] - bb[1], bb[4] - bb[2]), "2")

  if (missing(grid_gamma)) grid_gamma <- round(seq(min(gridmap$lx_ly), diagonal*frac, length.out = 15))
  if (missing(vars)) vars <- setdiff(names(grid), c("geom", "geometry", "id", "i"))

  dd <- gridmap$grid %>%
    st_drop_geometry %>%
    .[, vars, with = F] %>%
    as.matrix

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

  if (!is.null(sigma) & N>0) {
    ker <- cvmgof::kernel.function.quart(matriz/sigma) %>%
      Matrix::Matrix(sparse = T)
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t
    simulaciones <- crossprod(ker, simulaciones)
  }

  simulaciones <- cbind(dd, simulaciones)

  output <- sapply(grid_gamma, function(gamma) {

    ker <- cvmgof::kernel.function.quart(matriz/gamma) %>%
      Matrix::Matrix(sparse = T)
    ker <- apply(ker, 1, \(x) x/sum(x)) %>% t

    crossprod(ker, simulaciones)
  }, simplify = "array")

  output <- sapply(1:(N+1), \(i) {
    ind <- (1:length(vars)) + length(vars)*(i-1)
    output[,ind,]
  }, simplify = "array")

  dd_simulaciones <- sapply(1:(N+1), \(i) {
    ind <- (1:length(vars)) + length(vars)*(i-1)
    simulaciones[,ind]
  }, simplify = "array")


  results <- sapply(1:(N+1), function(rep) {
    sapply(1:length(grid_gamma), function(j){
      data <- dd_simulaciones[,,rep]
      ee <- output[,,j,rep]
      estimate_index(data, ee)$H
    })
  })
  # results <- apply(output, 3:4, function(ee) estimate_index(dd, ee)$H)
  rownames(results) <- grid_gamma

  if (N>1){
    results <- cbind(results[,1], t(apply(results[,-1], 1, quantile, prob = c(.01, .99))))
    colnames(results) <- c("H", ".01", ".99")

    m0 <- tryCatch({
      uniroot(approxfun(grid_gamma, results[,1] - results[,3]), interval = range(grid_gamma))$root
    }, error = function(foo) NULL)
    D <- min(m0, diagonal*.25) - lx
    gamma_micro <- lx + D/L
    gamma_macro <- lx + D*(L-1)/L
    Hmicro <- approxfun(grid_gamma, results[,1])(gamma_micro)
    Hmacro <- approxfun(grid_gamma, results[,1])(gamma_macro)

    # si el rango de grid_gamma es pequeño, pueden aparecer NA porque no exploro lo suficiente

    output <- list(results = results, grid_gamma = grid_gamma, values = c(Hmicro = Hmicro, Hmacro = Hmacro, gamma_micro = gamma_micro, gamma_macro = gamma_macro))
    class(output) <- "seg_profile"
  } else {
    colnames(results) <- "H"
    output <- results
  }

  return(output)
}
