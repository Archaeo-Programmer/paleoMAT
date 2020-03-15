modern_pollen_points <- function(x) {
  x %>%
    dplyr::select(sample.id, long, lat) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(coords = c("long","lat"),
                 crs = 4326)
}