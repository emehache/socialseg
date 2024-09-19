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
#' @return gridmap
#' @export

distribute <- function(input, lx, ly = lx, vars, bbox, crs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs", compute_distances = TRUE, ...) {

  if (missing(vars)) {
    vars <- setdiff(names(input), "geom")
  }

  input$id <- 1:nrow(input)

  # grid <- st_make_grid(input, cellsize = c(lx, ly), crs = st_crs(input))
  grid <- st_make_grid(input, cellsize = c(lx, ly), ...)

  # if (!missing(bbox)) grid <- st_make_grid2(input, bb = bbox, cellsize = c(lx, ly), ...)

  intersects <- st_intersects(input, grid) %>%
    lapply(as.data.table) %>%
    rbindlist(idcol = "id") %>%
    .[, id := as.numeric(id)] %>%
    setnames("V1", "i") %>%
    .[]

  data <- st_drop_geometry(input) %>%
    as.data.table() %>%
    .[, vars, with = F] %>%
    .[, id := .I] %>%
    .[]

  distributed_data <- intersects %>%
    .[, by = id, .(.N, i)] %>%
    merge(data) %>%
    .[ , (vars) := lapply(.SD, \(x) x/N), .SDcols = vars] %>%
    .[, keyby = i, lapply(.SD, sum), .SDcols = vars] %>%
    .[]

  grid <- as.data.table(grid) %>%
    .[, i:=1:.N] %>%
    .[] %>%
    merge(distributed_data) %>%
    st_as_sf()

  if (compute_distances){
    distances <- grid %>%
      st_geometry() %>%
      st_centroid() %>%
      st_distance(which = "Euclidean") %>%
      as.numeric %>%
      matrix(nrow(grid))
  } else {
    distances <- NULL
  }

  tol <- .Machine$double.eps

  dd <- grid %>%
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
  (H <- 1 - (sum(tot_p * ent_p) / (tot * E)))

  values <- list(H = H, tot = tot, E = E, tot_p = tot_p, ent_p = ent_p)

  output <- list(grid = grid, input = input, distances = distances, values = values, lx_ly = c(lx = lx, ly = ly))
  class(output) <- "gridmap"

  return(output)

}
