#' Distribute the data
#'
#' This function ...
#'
#' @param input Path to the input file
#' @return A data.table
#' @export

distribute <- function(input, Ngrid, lx, ly, vars, verbose = T){
  #Ngrid=50

  if (!missing(vars)) {
    if (length(vars) > 1) input@data <- input@data[, vars]
    else input@data <- as.data.frame(input@data[, vars]); names(input@data) <- vars
  }

  xy <- bbox(input)

  # Create a nrow * ncol grid point data set
  if (!missing(Ngrid)) {
    coords <- expand.grid(x = seq(xy[1,1], xy[1,2], length.out = Ngrid),
                          y = seq(xy[2,1], xy[2,2], length.out = Ngrid))
  }

  if (!missing(lx)) {
    if (missing(ly)) ly <- lx
    coords <- expand.grid(x = seq(xy[1,1], xy[1,2], by = lx),
                          y = seq(xy[2,1], xy[2,2], by = ly))
  }

  coords <- coords %>%
    as.data.table %>%
    .[, i := .I] %>%
    .[]

  coords_sp <- SpatialPoints(coords, proj4string = input@proj4string)

  CoordsInPoly <- over(input, coords_sp, returnList = T) %>%
    lapply(as.data.table) %>%
    rbindlist(idcol = "id") %>%
    .[, id := as.numeric(id)] %>%
    setnames("V1", "i") %>%
    .[]

  # id = identificador del poligono, de 1 a 780.
  # el i es el identificador del punto en la grilla

  coords <- merge(coords, CoordsInPoly, all.x = T)

  id_poly <- sapply(input@polygons, \(i) i@ID) %>%
    as.numeric


  # vars <- names(as.data.table(input))

  data <- input %>%
    rgeos::gCentroid(byid = T) %>%
    .@coords %>%
    cbind(as.data.table(input)) %>%
    .[, id := id_poly] %>%
    merge(CoordsInPoly) %>%
    .[,-c("x", "y")] %>%
    merge(coords, by = c("id", "i"), all.y = T) %>%
    .[!is.na(id), by = id, N := .N] %>%
    melt(id.vars = c(1:2, as.numeric(ncol(.))-2,as.numeric(ncol(.))-1,as.numeric(ncol(.)))) %>%
    # .[, value_pond := value/N/lx/ly] %>% # esto para que sea una densidad
    .[, value_pond := value/N] %>% # expresado en personas por cuadrado
    dcast(id + i + x + y + N ~ variable, value.var = "value_pond") %>%
    .[order(x,y)]

  # N es la cantidad de puntos de la grilla en cada poligono

  puntos_problema <- data[, by = .(x,y), .N] %>%
    .[N>1, !"N"] %>%
    merge(data, by = c("x", "y")) %>%
    .[, unique(i)]

  # vars <- names(input@data)
  data[i %in% puntos_problema, by = i,(vars) := lapply(.SD, mean), .SDcols = vars]
  data <- unique(data, by = c("x", "y", vars))


  if (verbose) {
    # id_faltantes <- setdiff(1:nrow(input),unique(data$id))
    id_faltantes <- setdiff(id_poly,unique(data$id))
    cat("Cantidad de poligonos no representados en la grilla:", length(id_faltantes), "\n")

    cat("Suma de las variables en los polígonos faltantes:\n")
    input %>%
      as.data.table %>%
      .[id_faltantes] %>%
      colSums(na.rm = T) %>%
      print
  }

  return(data)
}
