#' Estimate ...
#'
#' This function ...
#'
#' @param base Path to the input file
#'
#' @return A data.table
#'
#' @export

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x + 1, base)
  inv <- function(x) base^(-x) - 1
  trans_new(paste0("reverselog-", format(base)), trans, inv)
}

#' Estimate ...
#'
#' This function ...
#'
#' @param base input, var Path to the input file
#' @return A data.table
#' @export

log_trans <- function(base = exp(1)) {
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^(x) - 1
  trans_new(paste0("log-", format(base)), trans, inv)
}

#' Estimate ...
#'
#' This function ...
#'
#' @param data data
#' @param input polygon
#' @param var variable
#'
#' @return A data.table
#' @export

segplot <- function(data, input, var, poligono = T, contour = T, per = .9) {


  if (missing(data)) {
    vars <- names(input@data)
    if (missing(var)) var <- vars[1]

    X <- fortify(input) %>%
      as.data.table

    graf <- data.table(id = rownames(coordinates(input))) %>%
      .[, (var) := input@data[,var]] %>%
      .[] %>%
      merge(X) %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group, fill = .data[[var]]), col = "darkblue") +
      coord_fixed() +
      scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1))

    return(graf)

  }


  vars <- setdiff(names(data), c("id", "i", "x", "y", "N"))
  if (missing(var)) var <- vars[1]



  q <- quantile(data[[vars[1]]], per, na.rm = T)

  data2 <- copy(data)
  data2 %>%
    .[is.na(.[[var]]), (var) := 0]


  g <- data2 %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1)) +
    coord_equal() +
    geom_contour(aes(x = x, y = y, z = .data[[var]]), na.rm = F, breaks = q, col = 1,linewidth = 1, show.legend = T) +
    geom_polygon(data = input, aes(long, lat, group = group), linewidth = .08, alpha = .4, fill = NA, col = "darkblue") +
    theme_bw()

  contornos <- ggplot_build(g)$data[[2]] %>%
    as.data.table %>%
    .[,.(x,y, piece)]


  graf <- data %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    # geom_point(aes(col = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    # scale_fill_continuous(trans = reverselog_trans(), na.value = "transparent") +
    scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1)) +
    # scale_colour_continuous(trans = reverselog_trans(), na.value = "transparent") +
    coord_equal() +
    # ggtitle("Datos distribuidos con equal") +
    theme_bw()

  if(contour){
    graf <- graf +
      # stat_contour(aes(x = x, y = y, z = .data[[var]]), geom = "polygon", fill = NA, breaks = q, col = 1,linewidth = 1, show.legend = T)
      geom_path(data=contornos, aes(x,y,group = piece))
  }

  if (poligono) {
    graf <- graf +
      geom_polygon(data = input, aes(long, lat, group = group), linewidth = .08, alpha = .4, fill = NA, col = "darkblue")
  }

  # if ()

  return(graf)
}

# graficar(data %>% suavizar(sigma = 3), input, contour = T, poligono = T)

graficar2 <- function(data, var) {

  if (missing(var)) var <- names(as.data.table(input))[1]

  data %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    scale_fill_continuous(trans = reverselog_trans(), na.value = "transparent") +
    # geom_point(aes(col = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    # scale_colour_continuous(trans = reverselog_trans(), na.value = "transparent") +
    # geom_polygon(data = input, aes(long, lat, group = group), linewidth = .3, alpha = .7, fill = NA, col = "red") +
    # geom_contour(aes(x = x, y = y, z = nivel_edu_alto),
    # breaks = c(10), col = 1,linewidth = .7, show.legend = T) +
    coord_equal() +
    # ggtitle("Datos distribuidos con equal") +
    theme_bw()

}
