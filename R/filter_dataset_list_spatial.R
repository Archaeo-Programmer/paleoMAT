filter_dataset_list_spatial <- function(x, y){
  x %>%
    neotoma::get_site() %>%
    sf::st_as_sf(coords = c("long","lat"),
                 crs = 4326) %>%
    dplyr::mutate(dataset.id = names(x)) %>%
    sf::st_intersection(y %>%
                          sf::st_transform(4326))
}
