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

    # If you want to loan all 12 months, then you can use this as the first line rather than ("prism_extraction <- list(prism_7) %>%") below.
    # prism_extraction <- list(prism_1,
    #      prism_2,
    #      prism_3,
    #      prism_4,
    #      prism_5,
    #      prism_6,
    #      prism_7,
    #      prism_8,
    #      prism_9,
    #      prism_10,
    #      prism_11,
    #      prism_12) %>%

    # Here, to save some time for the extraction, we limit to just the month of July. If you want to run the full 12 months,
    # then the function can be altered like above.
    suppressWarnings(prism_extraction <- list(prism_7) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(element, month) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(extraction =
                      list(raster::extract(x = normal,
                                           y = sites))) %>%
      dplyr::select(element, month, extraction) %>%
      dplyr::ungroup() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sample.id =
                      list(sites$sample.id)) %>%
      dplyr::mutate(month = as.integer(month)) %>%
      tidyr::unnest(c(extraction, sample.id)) %>%
      # PRISM extractions are all multiplied by 10 in order to store them as integers.
      # Therefore, here, to get to mm for ppt, and deg_C for temperature, then divide by 10.
      dplyr::mutate(extraction = extraction / 10.00) %>%
      tidyr::pivot_wider(names_from = element,
                         values_from = extraction) %>%
      dplyr::left_join(month_days, by = "month") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tavg =
                      ((tmax + tmin) / 2))  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(gdd = (paleomat::calc_gdd(
        tmin = tmin,
        tmax = tmax,
        t.base = 10,
        t.cap = 30
      ) * days))  %>%
      dplyr::select(-days))

    # Find any rows with NAs then remove record from the sites dataset.
    NA_index <- which(is.na(prism_extraction$ppt))
    NA_index <- unique(prism_extraction$sample.id[NA_index])

    if(length(NA_index) != 0){
      sites <- sites %>%
        dplyr::filter(!sample.id %in% NA_index)
    }

    prism_extraction %>%
      # Drop any rows with NAs in the prism extraction.
      na.omit() %>%
      tidyr::nest(prism.normals = c(month,
                                    ppt,
                                    tmin,
                                    tmax,
                                    tavg,
                                    gdd)) %>%
      dplyr::right_join(sites, .) %>%
      sf::st_as_sf()

  }
