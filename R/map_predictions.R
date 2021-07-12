map_predictions <-
  function(site.preds,
           site.locs,
           raster.data,
           nfraction.df = NA,
           rast.extrap = FALSE) {
    # Prepare the raster dataset, so that the prediction can be done with the thin plate spline regression on the raster grid.
    # Create a bounding box around the site extent (i.e., for site.locs), but with a 1 degree buffer in each direction.
    bbox_buffer <- c(
      "xmin" = min(site.locs$long)-1,
      "ymin" = min(site.locs$lat)-1,
      "xmax" = max(site.locs$long)+1,
      "ymax" = max(site.locs$lat)+1
    )  %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf(crs = 4326) %>%
      sf::st_transform(crs = 4326)

    # Get the modern data (from PRISM) at each of the fossil pollen sites.
    modern.data <-
      as.data.frame(raster::extract(x = raster.data,
                                    y = site.preds %>%
                                      dplyr::select(long, lat))) %>%
      dplyr::rename(modern = 1)

    # Crop the raster dataset to the extent of the bounding box.
    raster.data <- raster::crop(raster.data, bbox_buffer)

    # Create a bounding box around the site extent (i.e., for site.locs). This is essentially the same as bbox_buffer, but without the 1 degree buffer.
    # This will be used to crop the raster output below.
    bbox_limited <- c(
      "xmin" = min(site.locs$long),
      "ymin" = min(site.locs$lat),
      "xmax" = max(site.locs$long),
      "ymax" = max(site.locs$lat)
    )  %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf(crs = 4326) %>%
      sf::st_transform(crs = 4326)


    # Need to extract the xy grid and put in ascending order, as the fields package expects that.
    # The top row or first record is NA, so removing the first row/record.
    raster.data.long <- raster::xFromCol(raster.data)
    raster.data.lat <- raster::yFromRow(raster.data) %>%
      sort()
    raster.data.2 <- raster::as.matrix(raster.data)
    # Transpose, so that rows and columns will match the long lat lists. Then, mirror the columns so that the latitude is ascending.
    raster.data.2 <- t(raster.data.2) %>%
      as.data.frame()
    raster.data.2 <- raster.data.2[, order(ncol(raster.data.2):1)] %>%
      as.matrix()
    # Put long, lat, and raster value into 1 list. Then, rename to x, y, and z.
    raster.data.list <-
      list(raster.data.long, raster.data.lat, raster.data.2)
    names(raster.data.list) <- c("x", "y", "z")

    #Create the grid list, which will be used in the prediction.
    grid.list <-
      list(x = raster.data.list$x, y = raster.data.list$y)

    # Next, get the number of sites, which is used to define the degrees of freedom (df).
    no.sites <- nrow(site.preds)

    site.preds <- cbind(site.preds, modern.data)

    if (is.null(nfraction.df) == TRUE) {
      stop(
        "Value for nfraction.df cannot be NULL. This function could not be completed. nfraction.df must be an integer
       between 0 and 1, or NA (the default). This is used to calculate the percentage of n that will be used to define df."
      )
    } else if (is.na(nfraction.df) == TRUE) {
      # Fit the model as the first step in the process.
      # Thin Plate Spline Regression
      fit_TPS <- fields::Tps(
        # Accepts points but expects them as matrix.
        x = as.matrix(site.preds[, c("long", "lat")]),
        # The dependent variable.
        Y = site.preds$value,
        # Modern temperature as an independent covariate.
        Z = site.preds$modern,
        miles = TRUE
      )
    } else if (nfraction.df < 0 | nfraction.df > 1) {
      stop(
        "Value for nfraction.df must be an integer between 0 and 1, or NA (the default). So, this function could not be completed.
       This is used to calculate the percentage of n that will be used to define df."
      )
    } else {
      # Fit the model as the first step in the process.
      # Thin Plate Spline Regression
      fit_TPS <- fields::Tps(
        # Accepts points but expects them as matrix.
        x = as.matrix(site.preds[, c("long", "lat")]),
        # The dependent variable.
        Y = site.preds$value,
        # Modern temperature as an independent covariate.
        Z = site.preds$modern,
        miles = TRUE,
        df = no.sites * nfraction.df
      )
    }

    if (rast.extrap == TRUE) {
      # Do prediction on raster surface and output a raster that extrapolates to the edge of the bounding box.
      fit.full <-
        fields::predictSurface(fit_TPS, grid.list, ZGrid = raster.data.list, extrap = TRUE)
      fit.full <- raster(fit.full)
      crs(fit.full) <- CRS('+init=EPSG:4326')

      fit.full.SE <-
        fields::predictSurfaceSE(
          fit_TPS,
          grid.list,
          ZGrid = raster.data.list,
          drop.Z = TRUE,
          extrap = TRUE
        )
      fit.full.SE <- raster(fit.full.SE)
      crs(fit.full.SE) <- CRS('+init=EPSG:4326')

      fit.full <- raster::crop(fit.full, bbox_limited)
      fit.full.SE <- raster::crop(fit.full.SE, bbox_limited)

    } else {
      # Do prediction on raster surface and output a raster that is a convex hull (i.e., no extrapolation beyond site.locs extent).
      fit.full <-
        fields::predictSurface(fit_TPS, grid.list, ZGrid = raster.data.list, extrap = FALSE)
      fit.full <- raster(fit.full)
      crs(fit.full) <- CRS('+init=EPSG:4326')

      fit.full.SE <-
        fields::predictSurfaceSE(
          fit_TPS,
          grid.list,
          ZGrid = raster.data.list,
          drop.Z = TRUE,
          extrap = FALSE
        )
      fit.full.SE <- raster(fit.full.SE)
      crs(fit.full.SE) <- CRS('+init=EPSG:4326')

      fit.full <- raster::crop(fit.full, bbox_limited)
      fit.full.SE <- raster::crop(fit.full.SE, bbox_limited)

    }

    return(list(fit.full, fit.full.SE))

  }
