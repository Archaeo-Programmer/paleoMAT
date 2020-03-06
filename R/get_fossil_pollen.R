get_fossil_pollen <- function(...){
  # Get the fossil pollen datasets for the United States and Canada, then download the sites.
  # Next, get the chronology data for each site, so that we can identify the core tops from each site (if available).
  # Next, use lapply to limit to the first data frame in the list of lists (each site has multiple dataframes associated with it);
  # so we limit to the chron.control dataframe. Next, we can strip away the upper most list, which is just the site ID,
  # but we are still left with "siteID.chron.control", as the name of the dataframe.

  NAfossil_datasets <-
    neotoma::get_dataset(datasettype = 'pollen', ...) %>%
    neotoma::get_download()

  NAfossil_pubs <-
    NAfossil_datasets %>%
    neotoma::get_publication()

  NAfossil_chron <-
    NAfossil_datasets %>%
    neotoma::get_chroncontrol()


  # Get the fossil pollen datasets in North America and use purrr::map to apply the compile function
  # to simply return a list of the metadata of each sample.
  # Then, combine the rows from the datasets together so that they are now in one tibble.
  NAfossil_metadata <-
    NAfossil_datasets %>%
    purrr::map_dfr(compile_pollen) %>%
    dplyr::mutate(type = "fossil") %>%
    dplyr::mutate(dataset.id = as.integer(dataset.id),
           site.id = as.integer(site.id))


  # Get earliest publication dates for each fossil pollen site.
  NAfossil_pub_date <-
    NAfossil_pubs %>%
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

  # Get the fossil datasets in North America and use purrr::map to add in counts, then change to a tibble, then bind rows together.
  # Then, use Neotoma's compile function for the Whitmore Full dataset of taxa, and finally change counts into a tibble.
  # We remove dataset.id because dplyr::left_join cannot process having multiple rows with the same dataset.id.
  NAfossil_counts <- NAfossil_datasets %>%
    purrr::map("counts") %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::map(neotoma::compile_taxa,
               list.name = "WhitmoreFull") %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::imap_dfr(.f = function(x,y){
      cbind(NAfossil_datasets[[y]]$sample.meta %>%
              dplyr::select(dataset.id, sample.id),
            x)
    }) %>%
    # select(-ends_with("dataset.id")) %>%
    dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 0, .))) %>%
    tibble::as_tibble()

  # Sort the taxa to be in alphabetical order.
  NAfossil_counts <- NAfossil_counts[,c(names(NAfossil_counts)[1:2], sort(names(NAfossil_counts)[3:ncol(NAfossil_counts)]))]

  # Check that Modern pollen dataset has the same columns as the fossil pollen dataset.
  # sameVariables <- function(x,y) {
  #     for (i in names(x)) {
  #         if (!(i %in% names(y))) {
  #             print('Warning: Names are not the same!')
  #             break
  #         }
  #         else if(i==tail(names(y),n=1)) {
  #             print('Names are identical.')
  #         }
  #     }
  # }
  #
  # sameVariables(NAfossil_counts, MPCT_counts_noNAs)

  # Now, we have three tibbles, one has the metadata, one has the publication year, and the other has the taxa and counts data. These are both in the same order, so now we can bind them together.

  # First, merge (using a left_join) the metadata with the publication year, using the dataset.id to match, just to double check.
  NAfossil_metadata_pub <- dplyr::left_join(NAfossil_metadata,
                                            NAfossil_pub_date,
                                            by  = "dataset.id")

  # The fossil counts are just bound to the metadata_pub because it is not possible to do a left join when multiple rows have the same dataset.id.
  NAfossil_metadata_counts <- dplyr::left_join(NAfossil_metadata_pub, NAfossil_counts,
                                               by = c("dataset.id" = "dataset.id","sample.id")) %>%
    dplyr::arrange(dataset.id)

  return(NAfossil_metadata_counts)
}
