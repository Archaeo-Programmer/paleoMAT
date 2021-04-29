simplify_reconstruction <- function(reconstruction.data, fossil.metadata){
  # In function(x), the reconstructed data is selected (or x$fit and the MAT column), then transformed into a tibble.
  # Then, the metadata is bound to the reconstructed data, which is for every sample for each month (months 1 through 12).
  # Then, the data is reorganized so that value (or the reconstructed tavg/GDD) is at the end.
  # The associated error of the climate reconstructions (or value) is added as an additional column.
  # map_dfr is used to bind all the rows together from the function being run on the 12 objects (or months of data).
  # .id is used to specify the month that the reconstruction and error belong to.
  # map_dfr is a cleaner way to represent the data than to combine all the columns together (using map_dfc),
  # then have to have 24 columns for both the climate reconstructions (i.e., value) and the associated error.
  # Finally, the geometry is converted to SF.
  reconstruction.data %>%
    purrr::map_dfr(function(x) {
      x$fit[, "MAT"] %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(fossil.metadata %>%
                           dplyr::select(dataset.id:pub_year)) %>%
        dplyr::select(dataset.id:geometry, value) %>%
        dplyr::mutate(error = x$SEP.boot[, "MAT"])
    },
    .id = "month") %>%
    sf::st_as_sf()
}
