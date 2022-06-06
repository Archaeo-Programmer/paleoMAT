map_predictions2 <-
  function(site.preds,
           grid.res,
           nfraction.df = NA) {
    # Prepare the raster dataset, so that the prediction can be done with the thin plate spline regression on the raster grid.
    # Create a bounding box around the site extent (i.e., for site.locs), but with a 1 degree buffer in each direction.

    bbox <- c(
      "xmin" = min(site.preds$long)-0.5,
      "ymin" = min(site.preds$lat)-0.5,
      "xmax" = max(site.preds$long)+0.5,
      "ymax" = max(site.preds$lat)+0.5
    )

    grd_template <- expand.grid(
      X = seq(from = bbox["xmin"], to = bbox["xmax"], by = grid.res),
      Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = grid.res) # degree resolution
    )

    grd_template_raster <- grd_template %>%
      dplyr::mutate(Z = 0) %>%
      raster::rasterFromXYZ(
        crs = 4326)

    # interp_TPS <- interpolate(grd_template_raster, fit_TPS)
    #
    #
    # df <- rasterToPoints(interp_TPS) %>% as_tibble()
    # colnames(df) <- c("X", "Y", "Z")
    #
    # sf_sites <- st_as_sf(site.preds, coords = c("long", "lat"), crs = 4326)
    #
    # ggplot(df, aes(x = X, y = Y, fill = Z)) +
    #   geom_raster() +
    #   ggtitle(label = "3350 BC") +
    #   scale_fill_viridis(option = "C") +
    #   geom_point(data = site.preds, aes(x = long, y = lat, fill = anom), inherit.aes = FALSE) +
    #   theme_bw()

    bbox_buffer <- c(
      "xmin" = min(site.preds$long)-0.5,
      "ymin" = min(site.preds$lat)-0.5,
      "xmax" = max(site.preds$long)+0.5,
      "ymax" = max(site.preds$lat)+0.5
    )  %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf(crs = 4326) %>%
      sf::st_transform(crs = 4326)

    # Crop the raster dataset to the extent of the bounding box.
    raster.data <- raster::crop(grd_template_raster, bbox_buffer)

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

    if (is.null(nfraction.df) == TRUE) {
      stop(
        "Value for nfraction.df cannot be NULL. This function could not be completed. nfraction.df must be an integer
       between 0 and 1, or NA (the default). This is used to calculate the percentage of n that will be used to define df."
      )
    } else if (is.na(nfraction.df)) {
      # Fit the model as the first step in the process.
      # Thin Plate Spline Regression
      fit_TPS <- fields::Tps(
        # Accepts points but expects them as matrix.
        x = as.matrix(site.preds[, c("long", "lat")]),
        # The dependent variable.
        Y = site.preds$anom,
        # Modern temperature as an independent covariate.
        #Z = site.preds$modern,
        miles = F
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
        Y = site.preds$anom,
        miles = F,
        df = no.sites * nfraction.df
      )
    }

    # Do prediction on raster surface and output a raster that extrapolates to the edge of the bounding box (i.e., the extent of the sites).
    fit.full <-
      fields::predictSurface(fit_TPS,
                             grid.list,
                             #ZGrid = raster.data.list,
                             extrap = F)
    fit.full <- raster(fit.full)
    crs(fit.full) <- CRS('+init=EPSG:4326')

    fit.full.SE <-
      fields::predictSurfaceSE(
        fit_TPS,
        grid.list,
        #ZGrid = raster.data.list,
        #drop.Z = TRUE,
        extrap = F
      )
    fit.full.SE <- raster(fit.full.SE)
    crs(fit.full.SE) <- CRS('+init=EPSG:4326')


    return(list(fit.full, fit.full.SE))

  }
