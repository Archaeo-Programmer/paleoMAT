#' @name BPtoBCAD
#' @title Convert BP dates to BC/AD format
#'
#' @description Converts calibrated BP dates to BC/AD dates
#'
#' @param x A numerical vector.
#' @return A vector with BC/BCE dates expressed as negative numbers and AD/CE dates as positive ones.
#' @examples
#' BPtoBCAD(5500)
#' @importFrom magrittr `%<>%` `%>%`
#' @importFrom stats na.omit
#' @export
BPtoBCAD <- function (x)
{
  index <- !is.na(x)
  res <- matrix(c(x, rep(NA, length(x))), ncol = 2)
  res[index & x < 1950, 2] <- 1950 - res[index & x < 1950,
                                         1]
  res[index & x >= 1950, 2] <- 1949 - res[index & x >= 1950,
                                          1]
  return(res[, 2])
}
