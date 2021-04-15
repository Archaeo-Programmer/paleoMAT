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

     list(prism_1,
         prism_2,
         prism_3,
         prism_4,
         prism_5,
         prism_6,
         prism_7,
         prism_8,
         prism_9,
         prism_10,
         prism_11,
         prism_12) %>%
       # This line is temporary as I'm just using prism_1 to test the script.
       # ok <- prism_1 %>%
       dplyr::bind_rows() %>%
       dplyr::arrange(element, month) %>%
       dplyr::rowwise() %>%
       #sf::st_transform(sites, crs = raster::projection(normal)) %>%
       dplyr::mutate(extraction =
                       list(raster::extract(x = normal,
                                            y = sites))) %>%
       dplyr::select(element, month, extraction) %>%
       dplyr::ungroup() %>%
       dplyr::mutate(month = as.integer(month)) %>%
       tidyr::unnest(extraction) %>%
       # PRISM extractions are all multiplied by 10 in order to store them as integers.
       # Therefore, here, to get to mm for ppt, and deg_C for temperature, then I divide by 10.
       dplyr::mutate(extraction = extraction / 10.00) %>%
       tidyr::pivot_wider(names_from = element,
                          values_from = extraction) %>%
       dplyr::left_join(month_days) %>%
       dplyr::mutate(gdd = calc_gdd(
         tmin = tmin,
         tmax = tmax,
         t.base = 10,
         t.cap = 30
       ) * days)  %>%
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
