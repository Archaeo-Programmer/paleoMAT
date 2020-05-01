extract_prism_normals <-
  function(sites) {

    # Calculate average days per month during 1961:1990
    month_days <-
      tibble::tibble(
        date = seq(lubridate::as_date("1961-01-01"),
                   lubridate::as_date("1990-12-31"),
                   "1 day")) %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date)) %>%
      dplyr::group_by(year, month) %>%
      dplyr::count() %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(`days` = mean(n))

    prism_normals %>%
      dplyr::mutate(extraction =
                      purrr::map(normal,
                                 function(x){
                                   ## Cast as VeloxRaster
                                   vx <- velox::velox(x)
                                   sites %>%
                                     # dplyr::select(sample.id, geometry) %>%
                                     dplyr::mutate(.,
                                                   extraction = as.numeric(vx$extract_points(.))) %>%
                                     sf::st_drop_geometry()
                                 })) %>%
      dplyr::select(element, month, extraction) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(month = as.integer(month)) %>%
      tidyr::unnest(extraction) %>%
      tidyr::pivot_wider(names_from = element,
                         values_from = extraction) %>%
      dplyr::left_join(month_days) %>%
      dplyr::mutate(gdd = calc_gdd(tmin = tmin,
                                   tmax = tmax,
                                   t.base = 10,
                                   t.cap = 30) * days)  %>%
      na.omit() %>%
      dplyr::select(-days) %>%
      tidyr::nest(prism.normals = c(month,
                                    ppt,
                                    tmin,
                                    tmax,
                                    gdd)) %>%
      dplyr::right_join(sites, .) %>%
      sf::st_as_sf()

  }
