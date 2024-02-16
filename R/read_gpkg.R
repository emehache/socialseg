#' Estimate ...
#'
#' This function ...
#'
#' @param file Path to the input file
#'
#' @return A data.table
#'
#' @export

read_gpkg <- function(file){
  input<-st_read(file, quiet = T)
  # if (loc20) input <- input %>%
  #     .[grep("*20$",.$LOCALIDAD),]
  input<-st_transform(input, crs=32721) %>%
    as_Spatial()
}
