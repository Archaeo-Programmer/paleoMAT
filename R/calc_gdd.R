#' @name calc_gdd
#' @title Calculate growing degree days
#'
#' @description Calculate growing degree days from PRISM temperature data
#'
#' @param tmin A numeric vector.
#' @param tmax A numeric vector.
#' @param t.base A numeric vector.
#' @param t.cap A numeric vector.
#' @return A vector with growing degree days.
#' @importFrom magrittr `%<>%` `%>%`
#' @importFrom stats na.omit
#' @export
calc_gdd <- function(tmin, tmax, t.base, t.cap = NULL) {
  if (length(tmin) != length(tmax)) {
    stop("tmin and tmax  must have same length!")
  }

  # Floor tmax and tmin at Tbase
  tmin[tmin < t.base] <- t.base
  tmax[tmax < t.base] <- t.base

  # Cap tmax and tmin at Tut
  if (!is.null(t.cap)) {
    tmin[tmin > t.cap] <- t.cap
    tmax[tmax > t.cap] <- t.cap
  }

  GDD <- ((tmin + tmax) / 2) - t.base

  return(GDD)
}
