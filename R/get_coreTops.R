#' @name get_coreTops
#' @title Get Core Top Data from Fossil Pollen Cores
#'
#' @description Get pollen core top sample numbers from fossil pollen cores
#'
#' @return A tibble with dataset ID, site ID, and sample ID.
#' @import httr
#' @importFrom magrittr `%<>%` `%>%` `%$%`
#' @export
get_coreTops <- function(){
  # Currently, to target the core top data from the fossil pollen (or "pollen") data,
  # I am using a service online ( https://tilia.neotomadb.org/retrieve/doc2/), which allows for me
  # to target the core tops through the SKOPE_GetSurfaceSampleData method (use selection #3 for core-top samples; 7 for surface samples).
  # Then, I copied the json file and used an online service to convert the json file to a csv, which I am reading in here.
  # Note: had to add a "wn" in front of tilia.neotomadb.org as the URL changed probably due to the transition to PostgreSQL.
  httr::GET("https://wntilia.neotomadb.org/retrieve/",
            query = list(method = "SKOPE_GetSurfaceSampleData",
                         DATASETTYPEID = 3,
                         EASTLONGBOUND = -100)) %>%
    httr::content() %$%
    data %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    dplyr::select(DatasetID, SiteID, SampleID) %>%
    dplyr::distinct()

}
