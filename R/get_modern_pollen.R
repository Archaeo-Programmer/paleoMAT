get_modern_pollen <- function(...){

  suppressWarnings(
    MP_datasets <-
      neotoma::get_dataset(datasettype='pollen surface sample', ...) %>%
      neotoma::get_download()
  )


  MP_pubs <- MP_datasets %>%
      neotoma::get_publication()

  # Get the modern pollen datasets in North America and use purrr::map to apply the compile function to simply return a list of the metadata of each sample.
  # Then, combine the rows from the datasets together so that they are now in one tibble.
  MP_metadata <- MP_datasets %>%
    purrr::map_dfr(paleomat::compile_pollen) %>%
    dplyr::mutate(type = "surface sample")

  MP_pub_date <- MP_pubs %>%
    purrr::map_dfr(.id = "dataset.id",
                   .f = function(x){
                     tibble::tibble(pub_year = x %>%
                                      purrr::map_dbl(function(y) y$meta$year) %>% # Get the year from every element of x
                                      as.integer()) # coerce to integer
                   }) %>%
    dplyr::mutate(pub_year = ifelse(is.na(pub_year),
                                    1990,
                                    pub_year),
                  dataset.id = as.integer(dataset.id)) %>%
    dplyr::group_by(dataset.id) %>%
    dplyr::slice(which.min(pub_year))

  # Get the modern pollen datasets in North America and use map to add in counts,
  # then change to a tibble, then bind rows together.
  # Then, use Neotoma's compile function for the Whitmore Full dataset of taxa,
  # and finally change counts into a tibble. With side IDs.
  MP_counts <- MP_datasets %>%
    purrr::map("counts") %>%
    purrr::map(as_tibble) %>%
    purrr::map(neotoma::compile_taxa,
               list.name = "WhitmoreFull") %>%
    purrr::map(as_tibble) %>%
    dplyr::bind_rows(.id = "dataset.id") %>%
    dplyr::mutate(dataset.id = as.integer(dataset.id))

  # Sort the taxa to be in alphabetical order.
  MP_counts <- MP_counts[,c(names(MP_counts)[1], sort(names(MP_counts)[2:ncol(MP_counts)]))]

  # Now, we have three tibbles, one has the metadata, one has the publication year, and the other has the taxa and counts data. These are both in the same order, so now we can bind them together.

  # First, merge the metadata with the publication year, using the dataset.id to match, just to double check.
  MP_metadata_counts <- dplyr::left_join(MP_metadata,
                                         MP_pub_date,
                                         by  = "dataset.id") %>%
    dplyr::left_join(MP_counts,
                     by  = "dataset.id") %>%
    dplyr::arrange(dataset.id)

  MP_metadata_counts

}
