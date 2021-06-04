map_predictions <-
  function(site.preds,
           degree.res,
           site.locs,
           rast.extent = TRUE,
           nfraction.df) {
    if (nfraction.df < 0 | nfraction.df > 1) {
      stop(
        "This function could not be completed. nfraction.df must be an integer between 0 and 1.
       This is used to calculate the percentage of n that will be used to define df."
      )
    }

    if (rast.extent == TRUE) {
      # First, prepare the raster grid, so that the interpolation can be done on the raster grid.
      # Create a bounding box around the site extent (i.e., for site.locs).
      bbox <- c(
        "xmin" = min(site.locs$long),
        "ymin" = min(site.locs$lat),
        "xmax" = max(site.locs$long),
        "ymax" = max(site.locs$lat)
      )

      grd_template <- expand.grid(
        x = seq(
          from = bbox["xmin"],
          to = bbox["xmax"],
          by = degree.res
        ),
        y = seq(
          from = bbox["ymin"],
          to = bbox["ymax"],
          by = degree.res
        ) # in degrees resolution
      )

      #Rasterize the grid.
      crs_raster_format <-
        "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

      grd_template_raster <- grd_template %>%
        dplyr::mutate(Z = 0) %>%
        raster::rasterFromXYZ(crs = crs_raster_format)

      # If extent is true, then a bounding box is created from the extent of the sites.
      # If it is false, then a polygon with a buffer is created.
    } else {
      # Get extent of fossil pollen sites and make a polygon.
      fossilpnts <- site.locs %>%
        sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
      fossilpolygon <- concaveman::concaveman(fossilpnts) %>%
        sf::st_as_sf(crs = 4326) %>%
        sf::st_transform(crs = 4326) %>%
        sf:::as_Spatial()

      # Next, apply a buffer 15 mile (or 24140.2 meters) buffer from the extent of the sites (to be the edge of extrapolation from the sites). Here, the projection is changed as gBuffer (or GEOS) expects planar coordinates. Then, these are converted back in order to use mask to clip the raster below.
      fossilpolygon <-
        sp::spTransform(fossilpolygon,
                        CRS("+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"))
      fossilpolygon <-
        rgeos::gBuffer(fossilpolygon, width = 24140.2)
      fossilpolygon <-
        sp::spTransform(fossilpolygon, CRS("+init=epsg:4326"))

      bbox <- c(
        "xmin" = min(site.locs$long) - (degree.res * 4),
        "ymin" = min(site.locs$lat) - (degree.res * 4),
        "xmax" = max(site.locs$long) + (degree.res * 4),
        "ymax" = max(site.locs$lat) + (degree.res * 4)
      )

      grd_template <- expand.grid(
        x = seq(
          from = bbox["xmin"],
          to = bbox["xmax"],
          by = degree.res
        ),
        y = seq(
          from = bbox["ymin"],
          to = bbox["ymax"],
          by = degree.res
        ) # in degrees resolution
      )

      #Rasterize the grid.
      crs_raster_format <-
        "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

      grd_template_raster <- grd_template %>%
        dplyr::mutate(Z = 0) %>%
        raster::rasterFromXYZ(crs = crs_raster_format)

      grd_template_raster <-
        raster::mask(grd_template_raster, fossilpolygon)

    }

    # Next, get the number of sites, which is used to define the degrees of freedom (df).
    no.sites <- nrow(site.preds)

    # Fit the model as the first step in the process.
    # Thin Plate Spline Regression
    fit_TPS <- fields::Tps(
      x = as.matrix(site.preds[, c("long", "lat")]),
      # accepts points but expects them as matrix
      Y = site.preds$anom,
      # the dependent variable
      #Z = ok$elev,
      miles = TRUE,
      df = no.sites * nfraction.df
    )

    interp_TPS <- interpolate(grd_template_raster, fit_TPS)
    crs(interp_TPS) <- CRS('+init=EPSG:4326')

    return(interp_TPS)

  }
