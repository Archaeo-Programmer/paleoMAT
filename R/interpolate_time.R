interpolate_time <-
  function(temp_data) {

    Q <- quantile(temp_data$anom, probs=c(.01, .99), na.rm = FALSE)
    iqr <- IQR(temp_data$anom)
    up <-  Q[2]+1.5*iqr # Upper Range
    low<- Q[1]-1.5*iqr # Lower Range
    eliminated <- subset(temp_data, temp_data$anom > (Q[1] - 1.5*iqr) & temp_data$anom < (Q[2]+1.5*iqr))

    gam_fit <- mgcv::gam(anom ~ s(date, bs = 'cs', k = (length(eliminated$anom)/2)), data = eliminated, family=gaussian())
    gam_knots <- gam_fit$smooth[[1]]$xp  ## extract knots locations

    agemax <- max(eliminated$date)
    agemax_rounded <- agemax %>%
      DescTools::RoundTo(multiple = 100, FUN = ceiling)
    agemin <- min(eliminated$date)
    agemin_rounded <- agemin %>%
      DescTools::RoundTo(multiple = 100, FUN = floor)

    predict.points <- as.data.frame(seq(agemin_rounded, agemax_rounded, by = 100))
    #predict.points <- as.data.frame(seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100))
    names(predict.points)[1] <- "date"

    predict.anom <- mgcv::predict.gam(gam_fit,predict.points,type='response')
    fit <- as.data.frame(predict.points,anom=predict.anom) %>%
      dplyr::filter(x >= -1000 & x <= 1800)

    return(fit)

  }
