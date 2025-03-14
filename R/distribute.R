#' Distribute the data over a grid
#'
#' `distribute` makes a regular grid of cells with sizes \code{c(lx,ly)}, divide the raw data stored in original polygons and distribute it over the grid.
#'
#' @param input Object of class \link{sf}.
#' @param lx,ly Numeric of length 1, with the size of gridcells.
#' @param vars Vector of variables names of data.
#' @param bbox Object of class \link{bbox}.
#' @param crs Coordinates reference system.
#' @param compute_distances Logical, if `TRUE` (default) the distance matrix between grid cells is calculated.
#' @param ... optional
#'
#' @return Object of class `gridmap`
#'
#' @export
#' @examples
#' \dontrun{
#'data(input)
#'distributed <- distribute(input = input, lx = 100, vars = c("nivel_edu_alto", "nivel_edu_bajo"))
#'plot(distributed)
#'}
# distribute <- function(input, lx, ly = lx, vars, bbox, crs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs", compute_distances = TRUE, ...) {
#
#   if (missing(vars)) {
#     vars <- setdiff(names(input), "geom")
#   }
#
#   input$id <- 1:nrow(input)
#
#   colnames <-  as.data.table(input)[, names(.SD), .SDcols = is.numeric]
#   input <- input %>%
#     as.data.table %>%
#     .[, (colnames) := lapply(.SD, nafill, fill = 0), .SDcols= is.numeric] %>%
#     .[] %>%
#     st_as_sf()
#
#
#   # grid <- st_make_grid(input, cellsize = c(lx, ly), crs = st_crs(input))
#   grid <- st_make_grid(input, cellsize = c(lx, ly))
#   grid <- st_intersection(grid, st_union(st_buffer(input, dist = .1)))
#
#
#
#
#   if (!missing(bbox)) grid <- st_make_grid2(input, bb = bbox, cellsize = c(lx, ly), ...)
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
#     # .[, by = .(i), lapply(.SD, sum), .SDcols = vars] %>%
#     .[, by = .(i, id), lapply(.SD, sum), .SDcols = vars] %>%
#     .[, by = .(i), lapply(.SD, mean), .SDcols = vars] %>%
#     .[]
#
#   grid <- as.data.table(grid) %>%
#     .[, i:=1:.N] %>%
#     .[] %>%
#     merge(distributed_data, all.x = T) %>%
#     # .[, by = i, (vars) := lapply(.SD, mean, na.rm = T), .SDcols = vars] %>% # agregado
#     st_as_sf %>%
#     st_cast("MULTIPOLYGON")
#
#   if (compute_distances){
#     distances <- grid %>%
#       st_geometry() %>%
#       st_centroid() %>%
#       st_distance(which = "Euclidean") %>%
#       as.numeric %>%
#       matrix(nrow(grid))
#   } else {
#     distances <- NULL
#   }
#
#   ### AGREGADO 20250221
#   na_index <- grid %>%
#     as.data.table %>%
#     na.omit(inver = T) %>%
#     .[,i]
#
#   grid <- na.omit(grid)
#
#   if (length(na_index) > 0) distances <- distances[-na_index, -na_index]
#
#   ### FIN AGREGADO 20250221
# #
# #   grid %>%
# #     as.data.table %>%
# #     .[, by = i, (vars) := lapply(.SD, mean), .SDcols = vars] %>%
# #     .[] %>%
# #     st_as_sf()
#
#
#
#   ######
#
#   tol <- .Machine$double.eps
#
#   dd <- grid %>%
#     as.data.table %>%
#     .[, vars, with = F]
#
#   # length vars debe ser mayor a 1
#   dd <- dd + tol
#   dd <- na.omit(dd)
#   M <- ncol(dd)
#   tot <- sum(dd)
#   tot_p <- rowSums(dd)
#   tot_m <- colSums(dd)
#   pi_m <- tot_m/tot
#   ent_p <- rowSums(-(dd/rowSums(dd))*log((dd/rowSums(dd)), base = M))
#   E <- -sum(pi_m * log(pi_m, base = M))
#   (H <- 1 - (sum(tot_p * ent_p) / (tot * E)))
#
#   values <- list(H = H, tot = tot, E = E, tot_p = tot_p, ent_p = ent_p)
#
#   output <- list(grid = grid, input = input, distances = distances, values = values, lx_ly = c(lx = lx, ly = ly), sigma = NULL)
#   class(output) <- "gridmap"
#
#   return(output)
#
# }



distribute <- function(input, lx, ly = lx, vars, bbox, crs = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs", compute_distances = TRUE, ...) {

  if (missing(vars)) {
    vars <- setdiff(names(input), "geom")
  }

  input$id <- 1:nrow(input)

  colnames <-  as.data.table(input)[, names(.SD), .SDcols = is.numeric]
  input <- input %>%
    as.data.table %>%
    .[, (colnames) := lapply(.SD, nafill, fill = 0), .SDcols= is.numeric] %>%
    .[] %>%
    st_as_sf()


  # grid <- st_make_grid(input, cellsize = c(lx, ly), crs = st_crs(input))
  grid <- st_make_grid(input, cellsize = c(lx, ly))
  grid <- st_intersection(grid, st_union(st_buffer(input, dist = .1)))




  if (!missing(bbox)) grid <- st_make_grid2(input, bb = bbox, cellsize = c(lx, ly), ...)

  #### agregado 20250312
  intersects <- st_intersects(input, st_centroid(grid)) %>%
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

  id_faltantes <- intersects %>%
    .[, by = id, .(.N, i)] %>%
    merge(data, all.y = T) %>%
    .[is.na(N), id]

  # lista <- st_intersects(input[input$id %in% id_faltantes,], grid) %>%
  #   lapply(as.data.table)
  # names(lista) <- id_faltantes
  #
  # intersects <- lista %>%
  #   rbindlist(idcol = "id") %>%
  #   .[, id := as.numeric(id)] %>%
  #   setnames("V1", "i") %>%
  #   .[] %>%
  #   rbind(intersects)

  intersects <- st_nearest_feature(input[input$id %in% id_faltantes,], st_centroid(grid)) %>%
    cbind(id = id_faltantes, i = .) %>%
    rbind(intersects, .) %>%
    .[order(id)]


  ### fin agregado 20250312

  distributed_data <- intersects %>%
    .[, by = id, .(.N, i)] %>%
    merge(data, all.y = T) %>%
    .[!is.na(N), (vars) := lapply(.SD, \(x) x/N), .SDcol = vars] %>%
    .[, by = .(i), lapply(.SD, sum), .SDcols = vars] %>%
    .[]


  grid <- as.data.table(grid) %>%
    .[, i:=1:.N] %>%
    .[] %>%
    merge(distributed_data, all.x = T) %>%
    # .[, by = i, (vars) := lapply(.SD, mean, na.rm = T), .SDcols = vars] %>% # agregado
    st_as_sf %>%
    st_cast("MULTIPOLYGON")

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

  ### AGREGADO 20250221
  na_index <- grid %>%
    as.data.table %>%
    na.omit(inver = T) %>%
    .[,i]

  grid <- na.omit(grid)

  if (length(na_index) > 0) distances <- distances[-na_index, -na_index]

  ### FIN AGREGADO 20250221
  #
  #   grid %>%
  #     as.data.table %>%
  #     .[, by = i, (vars) := lapply(.SD, mean), .SDcols = vars] %>%
  #     .[] %>%
  #     st_as_sf()



  ######

  tol <- .Machine$double.eps

  dd <- grid %>%
    as.data.table %>%
    .[, vars, with = F]

  # length vars debe ser mayor a 1
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

  output <- list(grid = grid, input = input, distances = distances, values = values, lx_ly = c(lx = lx, ly = ly), sigma = NULL)
  class(output) <- "gridmap"

  return(output)

}
