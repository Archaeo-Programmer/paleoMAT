map_predictions <-
  function(site.preds, prism.raster, preds.extent) {
    # First, prepare the raster data, so that the prediction can be done on the raster grid.
    # The PRISM raster data is at a very fine resolution, so the size of raster cells are increased slightly.
    rtemp <- raster::aggregate(prism.raster, fact = 2)
    icell <- 1:ncell(prism.raster)
    pred <- data.frame(
      value = rtemp[icell],
      cells = icell,
      x = xFromCell(rtemp, icell),
      y = yFromCell(rtemp, icell)
    )
    pred <- na.omit(pred)

    # pred <- pred %>%
    #   dplyr::mutate(value = value / 10.00)

    names(pred)[3:4] <- c("long", "lat")

    # Next, get the number of sites as mgcv::gam will throw an error message if the number of samples is too small (i.e., less than 30).
    # However, this can be circumvented by assigning a value for k for those time periods that are less than 30.
    # Here, the total number of sites is used when the number of sites is less than 30. If equal to or greater than 30, then the model
    # runs with the default settings.
    no.sites <- nrow(site.preds)
    # no.sites <- length(site.preds[[1]][[1]])

    if (no.sites < 30) {
      # For time periods with less than 30 sites, the total number of sites/samples are used for k.
      m_int <-
        mgcv::gam(anom ~ s(long, lat, k = no.sites), data = site.preds)

      #pred$elev <- 0
      pred$anom_pred <-
        predict(m_int, newdata = pred, type = "response")

      rpred <- raster::raster(rtemp)
      rpred[pred$cells] <- pred$anom_pred
      rpred_cropped <- raster::crop(x = rpred, y = preds.extent)

      return(rpred_cropped)

    } else {
      m_int <- mgcv::gam(anom ~ s(long, lat), data = site.preds)

      pred$anom_pred <-
        predict(m_int, newdata = pred, type = "response")

      rpred <- raster::raster(rtemp)
      rpred[pred$cells] <- pred$anom_pred
      rpred_cropped <- raster::crop(x = rpred, y = preds.extent)

      return(rpred_cropped)

    }

  }
