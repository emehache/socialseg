#' Estimate ...
#'
#' This function ...
#'
#' @param data data
#' @param vars variables
#'
#' @return A data.table
#'
#' @export

scale_density <- function(data, vars) {
  total <- data[, vars, with = F] %>%
    apply(1, sum)

  data[, total := total] %>%
    .[]

  data %>%
    .[total >0] %>%
    .[, (vars) := lapply(.SD, \(x) x/total), .SDcols = vars] %>%
    .[]
}
