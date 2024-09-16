#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return gridmap
#' @export

# distribute <- function(input, lx, ly = lx, vars, crs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs", ...) {
#
#   if (missing(vars)) {
#     vars <- setdiff(names(input), "geom")
#   }
#
#   # if (missing(xy)) {
#   #   xy <- st_bbox(input) %>%
#   #     matrix(2,2)
#   # }
#
#   input$id <- 1:nrow(input)
#
#   # grid <- st_make_grid(input, cellsize = c(lx, ly), crs = st_crs(input))
#   grid <- st_make_grid(input, cellsize = c(lx, ly), ...)
#
#   intersects <- st_intersects(input, grid) %>%
#     lapply(as.data.table) %>%
#     rbindlist(idcol = "id") %>%
#     .[, id := as.numeric(id)] %>%
#     setnames("V1", "i") %>%
#     .[]
#
#   data <- st_drop_geometry(input) %>%
#     as.data.table() %>%
#     .[, vars, with = F] %>%
#     .[, id := .I] %>%
#     .[]
#
#   distributed_data <- intersects %>%
#     .[, by = id, .(.N, i)] %>%
#     merge(data) %>%
#     .[ , (vars) := lapply(.SD, \(x) x/N), .SDcols = vars] %>%
#     .[, keyby = i, lapply(.SD, sum), .SDcols = vars] %>%
#     .[]
#
#   grid <- as.data.table(grid) %>%
#     .[, i:=1:.N] %>%
#     .[] %>%
#     merge(distributed_data) %>%
#     st_as_sf()
#
#   return(grid)
#
# }


distribute <- function(input, lx, ly = lx, vars, crs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs", compute_distances = TRUE, ...) {

  if (missing(vars)) {
    vars <- setdiff(names(input), "geom")
  }

  # if (missing(xy)) {
  #   xy <- st_bbox(input) %>%
  #     matrix(2,2)
  # }

  input$id <- 1:nrow(input)

  # grid <- st_make_grid(input, cellsize = c(lx, ly), crs = st_crs(input))
  grid <- st_make_grid(input, cellsize = c(lx, ly), ...)

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

  output <- list(grid = grid, input = input, distances = distances, values = values)
  class(output) <- "gridmap"

  return(output)

}
