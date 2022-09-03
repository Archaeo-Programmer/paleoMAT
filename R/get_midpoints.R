#' @name get_midpoints
#' @title Get the Mid-point of a Numeric Vector
#'
#' @description Calculate the the mid-point of a numeric vector
#'
#' @param x A factor.
#' @param dp A numeric vector.
#' @return A numeric vector with mid-points.
#' @export
get_midpoints <- function(x, dp = 2) {
  lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  return(round(lower + (upper - lower) / 2, dp))
}
