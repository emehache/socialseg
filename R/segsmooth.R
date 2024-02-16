#' Smooth the data
#'
#' This function ...
#'
#' @param data, sigma, ...
#' @return A data.table
#' @export

segsmooth <- function(data, sigma, bleed = F, normalise = F, rescale = T) {

  vars <- setdiff(names(data), c("id", "i", "x", "y", "N"))

  sigma <- (sigma-.9)*sqrt(1/7-.01)

  out <- lapply(vars, function(var) {
    M <- data %>%
      dcast(x ~ y, value.var = var) %>%
      .[, !"x"] %>%
      as.matrix %>%
      as.im %>%
      blur(sigma, bleed = bleed, normalise = normalise, kernel = "quartic") %>%
      as.matrix()
    dimnames(M) <- list(unique(data$x), unique(data$y))
    M <- M %>%
      as.data.table(keep.rownames = "x") %>%
      melt(id = "x", variable.name = "y", value.name = var) %>%
      .[, y := as.character(y)] %>%
      .[, lapply(.SD, as.numeric)] %>%
      .[, (var) := sapply(.SD, \(i) fifelse(i >=0, i, 0)), .SDcols = var] %>%
      .[]
  }) %>% Reduce(f = merge)


  s1 <- data[, colSums(.SD, na.rm = T), .SDcols = vars]
  s2 <- out[, colSums(.SD, na.rm = T), .SDcols = vars]
  frac <- s1/s2

  if (rescale) {
    lapply(vars, function(var) {
      out[, (var) := .SD*frac[var], .SDcols = var] %>%
        .[]
    }) %>% invisible()
  }

    return(out)

}
