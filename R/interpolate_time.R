interpolate_time <-
  function(temp_data, model = "tps") {
    # Only the most extreme outliers are removed here by using 0.01 and 0.99 quantiles.
    #First, if there are too few data points, then the input is just returned.
    Q <- quantile(temp_data$anom,
                  probs = c(.01, .99),
                  na.rm = FALSE)

    if (anyNA(Q) == TRUE) {
      eliminated <- as.data.frame(temp_data)

    } else if (is.na(nfraction.df) == TRUE) {
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
      model_fit <- lm(anom ~ date, data = eliminated)

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
        as.data.frame(predict(model_fit, predict.points, se.fit = TRUE)) %>%
        dplyr::rename(anom = 1)

      fit <- cbind(predict.points, predict.anom)

      return(list(fit, model_fit))

    } else if (model == "gam") {
      # If there are at least 3 samples, then run the mgcv::gam model. First, get the smoothing dimension, which is generally n/2.
      # However, for mgcv::gam, k must be at least equal to 3. So, for any n length less than 6, then the min number is 3.
      if (length(eliminated$anom) < 6) {
        smooth.dim <- 3

      } else {
        smooth.dim <- ceiling(length(eliminated$anom) * 0.6)

      }

      model_fit <-
        mgcv::gam(anom ~ s(date, bs = 'cs', k = smooth.dim),
                  data = eliminated,
                  family = gaussian())
      #plot(model_fit)
      gam_knots <-
        model_fit$smooth[[1]]$xp  ## extract knots locations
      # Can also get more basic data for plot by saving the plot.gam as an object. Then, could save something like the standard error.
      #plot_gam <- plot.gam(model_fit)
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
          model_fit,
          predict.points,
          type = 'response',
          se.fit = TRUE
        )) %>%
        dplyr::rename(anom = 1)
      fit <- cbind(predict.points, predict.anom)

      return(list(fit, model_fit))

    } else if (model == "tps") {
      if (length(eliminated$anom) == 3) {
        # Tps cannot run when n = 3. Here, we set it to 3 because above we have already dealt with when n<=2, which results in a lm.
        fit <- interpolate_time_natural(eliminated)

      } else {
        if (length(eliminated$anom) > 3 & length(eliminated$anom) <= 6) {
          smooth.dim <- 4

        } else {
          smooth.dim <- ceiling(length(eliminated$anom) * 0.6)
        }

        model_fit <-
          fields::Tps(eliminated$date, eliminated$anom, df = smooth.dim)
        int_grid <-
          round((max(model_fit$x) - min(model_fit$x)) / 100)
        xgrid <- seq(min(model_fit$x), max(model_fit$x), , int_grid)
        fhat <- predict(model_fit, xgrid)
        # plot(eliminated$date ,eliminated$anom)
        # title('Tps')
        # lines(xgrid, fhat,)
        SE <- fields::predictSE(model_fit, xgrid)
        # lines(xgrid,fhat + 1.96* SE, col="red", lty=2)
        # lines(xgrid, fhat - 1.96*SE, col="red", lty=2)
        fit <-
          cbind(as.data.frame(xgrid),
                as.data.frame(fhat),
                as.data.frame(SE))
        names(fit) <- c("date", "anom", "se.fit")

      }
      return(list(fit, model_fit))

    } else if (model == "natural") {

      fit <- interpolate_time_natural(eliminated)

      return(list(fit[[1]], model_fit[[2]]))

    } else {

      stop("Please choose a valid model.")
    }

  }
