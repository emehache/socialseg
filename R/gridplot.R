reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x + 1, base)
  inv <- function(x) base^(-x) - 1
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv)
}

log_trans <- function(base = exp(1)) {
  trans <- function(x) log(x + .03, base)
  inv <- function(x) base^(x) - .03
  scales::trans_new(paste0("log-", format(base)), trans, inv)
}



gridplot <- function(datos, # data.table/data.frame
                     input, # poligono
                     var, poligono = T, contour = F, per = .9, limits,
                     nombre_var) {

  # Datos es opcional SII le ponemos "poligono_distribuido" como input
  # del mismo modo, input podria ser opcional cuando le pones datos y no te interesa graficar el poligono original

  if (missing(nombre_var)) nombre_var <- var

  if (missing(datos)) {
    vars <- names(input@data)
    if (missing(var)) var <- vars[1]

    X <- fortify(input) %>%
      as.data.table

    graf <-
      data.table(id = rownames(coordinates(input))) %>%
      .[, (var) := input@data[,var]] %>%
      .[] %>%
      merge(X) %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group, fill = .data[[var]])) +
      # geom_contour(aes(x = long, y = lat, z = .data[[var]]), na.rm = F, breaks = q, col = 1,linewidth = 10, show.legend = T) +
      coord_fixed()
    # scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1))

    if (poligono) {
      graf <- graf +
        geom_polygon(data = input, aes(long, lat, group = group), linewidth = .08, alpha = .4, fill = NA, col = "darkblue")
    }

    if (contour) {

      datos <- graf$data %>% as.data.table
      q <- quantile(datos[[var]], per, na.rm = T)

      graf <- graf +
        geom_polygon(data = datos[datos[[var]] > q], aes(long, lat, group = group), fill = 1, alpha = .5, col = "transparent")

    }

    if (missing(limits)) {
      graf <- graf +
        scale_fill_gradient(name = nombre_var, low = "#FEF001", high = "#F00505",
                            na.value = "transparent", trans = log_trans(base = 1.1),
                            labels=function(x) x)
    } else {
      graf <- graf +
        scale_fill_gradient(name = nombre_var, low = "#FEF001", high = "#F00505", na.value = "transparent", #trans = log_trans(base = 1.1),
                            limits = limits,
                            labels=function(x) x)
    }

    return(graf)

  }


  vars <- setdiff(names(datos), c("id", "i", "x", "y", "N"))

  if (missing(var)) var <- vars[1]


  q <- quantile(datos[[var]], per, na.rm = T)

  datos2 <- copy(datos) %>%
    as.data.table %>%
    .[is.na(.[[var]]), (var) := 0]

  if (contour) {
    g <- datos2 %>%
      ggplot(aes(x, y)) +
      geom_tile(aes(fill = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
      # scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1)) +
      coord_equal() +
      geom_contour(aes(x = x, y = y, z = .data[[var]]), na.rm = T, breaks = q, col = 1,linewidth = 10, show.legend = T) +
      geom_polygon(data = input, aes(long, lat, group = group), linewidth = 0.08, alpha = .4, fill = NA, col = "darkblue") +
      theme_bw()

    contornos <- ggplot_build(g)$data[[2]] %>%
      as.data.table %>%
      .[,.(x,y, piece)]
  }

  graf <- datos %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    # geom_point(aes(col = .data[[var]]), alpha = .7) + # toquetea aca el alpha si queres que los colores sean mas transparentes
    # scale_fill_continuous(trans = reverselog_trans(), na.value = "transparent") +
    # scale_fill_gradient(low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1)) +
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

  if (missing(limits)) {
    graf <- graf +
      scale_fill_gradient(name = nombre_var, low = "#FEF001", high = "#F00505", na.value = "transparent", trans = log_trans(base = 1.1),
                          labels=function(x) x)
  } else {
    graf <- graf +
      scale_fill_gradient(name = nombre_var, low = "#FEF001", high = "#F00505", na.value = "transparent",
                          trans = log_trans(base = 4),
                          limits = limits,
                          labels=function(x) x)
  }




  return(graf)
}
