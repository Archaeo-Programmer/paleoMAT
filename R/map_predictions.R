map_predictions <-
  function(site.preds, prism.raster) {

    m_int <- mgcv::gam(anom ~s(long, lat), data = site.preds)
    rsst2 <- aggregate(prism.raster, 2)
    icell <- 1:ncell(prism.raster)
    pred <- data.frame(value = rsst2[icell],
                       cells = icell,
                       x = xFromCell(rsst2, icell),
                       y = yFromCell(rsst2, icell))
    pred <- na.omit(pred)

    pred <- pred %>%
      dplyr::mutate(value = value / 10.00)

    names(pred)[3:4] <- c("long", "lat")
    pred$anom_pred <- predict(m_int, newdata = pred, type = "response")

    rpred <- raster(rsst2)
    rpred[pred$cells] <- pred$richness_pred
    rpred_cropped <- crop(x = rpred, y = bb2)

  }
