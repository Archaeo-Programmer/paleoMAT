#' PRISM Climate for July Dataset
#'
#' The fossil and modern pollen datasets were originally downloaded from the
#' Neotoma Paleoecology Database. We processed and cleaned those data in
#' UUSS_MAT_Reconstruction.Rmd. The climate data was acquired from PRISM.
#'
#' @format An object of class `tibble`.
#' @source \url{https://prism.oregonstate.edu/}
"prism_7"

#' Fossil Pollen dataset for United States, Canada, and Mexico.
#'
#' A `tibble` containing the fossil pollen dataset for United States, Canada, and Mexico downloaded from Neotoma using the neotoma r package.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"NAfossil_metadata_counts"

#' Modern Pollen dataset for United States, Canada, and Mexico.
#'
#' A `tibble` containing the modern pollen dataset for United States, Canada, and Mexico downloaded from Neotoma using the neotoma r package.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"MP_metadata_counts"

#' Core tops dataset extracted from the fossil pollen dataset.
#'
#' A `tibble` containing the core tops samples for United States, Canada, and Mexico downloaded from Tilia in Neotoma.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"coreTops"

#' Fossil pollen dataset for SWUS across all time.
#'
#' A `tibble` containing proportional fossil pollen for the SWUS.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"fossil_pollen"

#' Fossil pollen dataset for SWUS for post 5500 BP.
#'
#' A `tibble` containing the fossil pollen dataset for SWUS for post 5500 BP and that have ages for each sample.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"fossil_west_post5500"

#' Modern pollen dataset for western United States.
#'
#' A `tibble` containing proportional modern pollen for western United States.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"MPCT_proportions"

#' Richard Alley’s (2000) Greenland ice core data.
#'
#' A `tibble` containing Richard Alley’s (2000) Greenland ice core data.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.ncei.noaa.gov/pub/data/paleo/icecore/greenland/summit/gisp2/isotopes/gisp2_temp_accum_alley2000.txt}
"alley_2000"

#' Kaufman et al. (2020) terrestrial composite for 30–60 °N.
#'
#' A `tibble` containing Kaufman et al. (2020) terrestrial composite for 30–60 °N.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.ncei.noaa.gov/access/paleo-search/study/27330}
"kaufman_2020"

#' Osman et al. (2021) globally resolved surface temperatures data.
#'
#' A `tibble` containing Osman et al. (2021) globally resolved surface temperatures data.
#'
#' @format An object of class `tibble`.
#' @source \url{https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-021-03984-4/MediaObjects/41586_2021_3984_MOESM3_ESM.xlsx}
"osman_2021"

#' PAGES 2k Consortium (2019) temperature anomalies (50th percentile) with respect to 1961–1990 CE.
#'
#' A `tibble` containing PAGES 2k Consortium (2019) temperature anomalies (50th percentile) with respect to 1961–1990 CE.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.ncei.noaa.gov/pub/data/paleo/pages2k/neukom2019temp/recons/Full_ensemble_median_and_95pct_range.txt}
"pages2k_2019"

#' Viau et al. (2006) North American July temperature reconstruction.
#'
#' A `tibble` containing Viau et al. (2006) North American July temperature reconstruction.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.ncei.noaa.gov/pub/data/paleo/pollen/recons/northamerica/viau2006namerica-temp.xls}
"viau_2006"

#' Routson et al. (2021) summer temperature composite (500-year bins) for Western North America.
#'
#' A `tibble` containing Routson et al. (2021) summer temperature composite (500-year bins) for Western North America.
#'
#' @format An object of class `tibble`.
#' @source \url{https://doi.org/10.6084/m9.figshare.12863843.v1}
"routson_2021"

#' Moberg et al. (2005) Northern Hemisphere low-frequency component.
#'
#' A `tibble` containing Moberg et al. (2005) Northern Hemisphere low-frequency component.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/moberg2005/nhtemp-moberg2005.txt}
"moberg_2005"

#' State boundaries for USA.
#'
#' A `Polygons` of United States' state boundaries.
#'
#' @format An object of class `SpatialPolygonsDataFrame`.
#' @source \url{https://www.sciencebase.gov/catalog/item/4f70b219e4b058caae3f8e19}
"states"

#' Pollen range for western part of the US.
#'
#' A `MULTIPOLYGON` of western pollen ranges as defined by [Williams and Shuman 2008](https://doi.org/10.1016/j.quascirev.2008.01.004), which had eastern and western pollen splits.
#'
#' @format An object of class `sfc_MULTIPOLYGON`.
#' @source \url{https://ars.els-cdn.com/content/image/1-s2.0-S0277379108000061-mmc1.zip}
"west_in_states"

#' Bacon Age-depth Models.
#'
#' A `tibble` containing  Bacon age models for fossil pollen sites.
#'
#' @format An object of class `tibble`.
#' @source \url{https://www.neotomadb.org/}
"bacon_age_models"

#' Mean July Temperature from PRISM (1961-1990).
#'
#' A `raster` containing the mean July temperature for 1961-1990 from PRISM for the southwestern US.
#'
#' @format An object of class `RasterLayer`.
#' @source \url{http://www.prism.oregonstate.edu/}
"temp.raster"
