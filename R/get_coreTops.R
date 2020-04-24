get_coreTops <- function(){

  # Currently, to target the core top data from the fossil pollen (or "pollen") data,
  # I am using a service online ( https://tilia.neotomadb.org/retrieve/doc2/), which allows for me
  # to target the core tops through the SKOPE_GetSurfaceSampleData method (use selection #3 for core-top samples; 7 for surface samples).
  # Then, I copied the json file and used an online service to convert the json file to a csv, which I am reading in here.
  httr::GET("https://tilia.neotomadb.org/retrieve/",
            query = list(method = "SKOPE_GetSurfaceSampleData",
                         DATASETTYPEID = 3,
                         EASTLONGBOUND = -100)) %>%
    httr::content() %$%
    data %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    dplyr::select(DatasetID, SiteID, SampleID) %>%
    dplyr::distinct()

}
