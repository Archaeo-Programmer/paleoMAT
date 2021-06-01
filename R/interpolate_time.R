interpolate_time <-
  function(temp_data) {
    # Only the most extreme outliers are removed here by using 0.01 and 0.99 quantiles.
    #First, if there are too few data points, then the input is just returned.
    Q <- quantile(temp_data$anom,
                  probs = c(.01, .99),
                  na.rm = FALSE)

    if (anyNA(Q) == TRUE) {
      eliminated <- as.data.frame(temp_data)

    } else {
      iqr <- IQR(temp_data$anom)
      up <-  Q[2] + 1.5 * iqr # Upper Range
      low <- Q[1] - 1.5 * iqr # Lower Range
      eliminated <-
        subset(temp_data,
               temp_data$anom > (Q[1] - 1.5 * iqr) &
                 temp_data$anom < (Q[2] + 1.5 * iqr))

    }

    # First, check to see if there is enough data to do a cubic spline. If not, then do a simple linear regression,
    # and predict on the linear regression.
    if (nrow(eliminated) <= 2) {
      gam_fit <- lm(anom ~ date, data = eliminated)

      agemax <- max(eliminated$date)
      agemax_rounded <- agemax %>%
        DescTools::RoundTo(multiple = 100, FUN = ceiling)
      agemin <- min(eliminated$date)
      agemin_rounded <- agemin %>%
        DescTools::RoundTo(multiple = 100, FUN = floor)

      predict.points <-
        as.data.frame(seq(agemin_rounded, agemax_rounded, by = 100))
      #predict.points <- as.data.frame(seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100))
      names(predict.points)[1] <- "date"

      predict.anom <-
        as.data.frame(predict(gam_fit, predict.points, se.fit = TRUE)) %>%
        dplyr::rename(anom = 1)

      fit <- cbind(predict.points, predict.anom)

      return(list(fit, gam_fit))

    } else {
      # If there are at least 3 samples, then run the mgcv::gam model. First, get the smoothing dimension, which is generally n/2.
      # However, for mgcv::gam, k must be at least equal to 3. So, for any n length less than 6, then the min number is 3.
      if (length(eliminated$anom) < 6) {
        smooth.dim <- 3

      } else {
        smooth.dim <- ceiling(length(eliminated$anom) *0.6)

      }

      gam_fit <-
        mgcv::gam(anom ~ s(date, bs = 'cs', k = smooth.dim),
                  data = eliminated,
                  family = gaussian())
      #plot(gam_fit)
      gam_knots <-
        gam_fit$smooth[[1]]$xp  ## extract knots locations
      # Can also get more basic data for plot by saving the plot.gam as an object. Then, could save something like the standard error.
      #plot_gam <- plot.gam(gam_fit)
      #plot_gam[[1]]$se

      agemax <- max(eliminated$date)
      agemax_rounded <- agemax %>%
        DescTools::RoundTo(multiple = 100, FUN = ceiling)
      agemin <- min(eliminated$date)
      agemin_rounded <- agemin %>%
        DescTools::RoundTo(multiple = 100, FUN = floor)

      predict.points <-
        as.data.frame(seq(agemin_rounded, agemax_rounded, by = 100))
      #predict.points <- as.data.frame(seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100))
      names(predict.points)[1] <- "date"

      predict.anom <-
        as.data.frame(mgcv::predict.gam(
          gam_fit,
          predict.points,
          type = 'response',
          se.fit = TRUE
        )) %>%
        dplyr::rename(anom = 1)
      fit <- cbind(predict.points, predict.anom)

      return(list(fit, gam_fit))
    }
  }
