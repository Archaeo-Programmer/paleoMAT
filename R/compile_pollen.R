#' @name compile_pollen
#' @title Compile Modern Pollen Data
#'
#' @description Compile modern pollen data into a `tibble`
#'
#' @param x A download_list from Neotoma.
#' @return A tibble with modern pollen data.
#' @importFrom magrittr `%<>%` `%>%`
#' @export

# Compile function for the modern pollen data from Neotoma.
compile_pollen <- function(x){
  tibble::tibble(
    dataset.id = x$dataset$dataset.meta$dataset.id,
    site.id = x$dataset$site.data$site.id,
    sample.id = x$sample.meta$sample.id,
    site.name = x$dataset$site$site.name,
    depth = x$sample.meta$depth,
    lat = x$dataset$site$lat,
    long = x$dataset$site$long,
    elev = x$dataset$site$elev,
    age = x$sample.meta$age)
}
