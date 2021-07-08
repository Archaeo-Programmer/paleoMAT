interpolate_time <-
  function(temp_data,
           model = "tps",
           gam.smooth = NULL) {
    # Only the most extreme outliers are removed here by using 0.01 and 0.99 quantiles.
    #First, if there are too few data points, then the input is just returned.
    Q <- quantile(temp_data$value,
                  probs = c(.01, .99),
                  na.rm = FALSE)

    if (anyNA(Q) == TRUE) {
      eliminated <- as.data.frame(temp_data)

    } else {
      iqr <- IQR(temp_data$value)
      up <-  Q[2] + 1.5 * iqr # Upper Range
      low <- Q[1] - 1.5 * iqr # Lower Range
      eliminated <-
        subset(temp_data,
               temp_data$value > (Q[1] - 1.5 * iqr) &
                 temp_data$value < (Q[2] + 1.5 * iqr))

    }

    # Here, we set up the midpoint years of each century from the minimum to maximum year represented by each site.
    agemax <- max(eliminated$date)
    agemax_rounded <- agemax %>%
      DescTools::RoundTo(multiple = 100, FUN = ceiling)
    agemin <- min(eliminated$date)
    agemin_rounded <- agemin %>%
      DescTools::RoundTo(multiple = 100, FUN = floor)

    # predict.points <-
    #   as.data.frame(seq(agemin_rounded, agemax_rounded, by = 100))
    predict.points <-
      as.data.frame(seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100))
    names(predict.points)[1] <- "date"

    # First, check to see if there is enough data to do a cubic spline. If not, then do a simple linear regression,
    # and predict on the linear regression.
    if (nrow(eliminated) <= 2) {
      model_fit <- lm(value ~ date, data = eliminated)

      predict.value <-
        as.data.frame(predict(model_fit, predict.points, se.fit = TRUE)) %>%
        dplyr::rename(value = 1)

      fit <- cbind(predict.points, predict.value)

      # Now, plot the results. Save as a ggplot object and will return with the rest of the data and models.
      plot_fit <-
        interpolate_time_plots(fit, eliminated, agemin_rounded, agemax_rounded, std.err = FALSE)

      return(list(fit, model_fit, plot_fit))

    } else if (model == "gam") {
      if (is.null(gam.smooth) == TRUE) {
        stop(
          "You must select a smooth term for GAM. Please see https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html for options."
        )
      } else {
        # If there are at least 3 samples, then run the mgcv::gam model. First, get the smoothing dimension, which is generally n/2.
        # However, for mgcv::gam, k must be at least equal to 3. So, for any n length less than 6, then the min number is 3.
        if (length(eliminated$value) < 6) {
          smooth.dim <- 3

        } else {
          smooth.dim <- ceiling(length(eliminated$value) * 0.6)

        }

        model_fit <-
          mgcv::gam(
            value ~ s(date, bs = gam.smooth, k = smooth.dim),
            data = eliminated,
            family = gaussian()
          )
        #plot(model_fit)
        gam_knots <-
          model_fit$smooth[[1]]$xp  ## extract knots locations
        # Can also get more basic data for plot by saving the plot.gam as an object. Then, could save something like the standard error.
        #plot_gam <- plot.gam(model_fit)
        #plot_gam[[1]]$se

        predict.points <- as.data.frame(predict.points) %>%
          dplyr::rename(date = predict.points)

        predict.value <-
          as.data.frame(mgcv::predict.gam(
            model_fit,
            predict.points,
            type = 'response',
            se.fit = TRUE
          )) %>%
          dplyr::rename(value = 1)

        fit <- cbind(predict.points, predict.value)

        plot_fit <-
          interpolate_time_plots(fit,
                                 eliminated,
                                 agemin_rounded,
                                 agemax_rounded,
                                 std.err = TRUE)

        return(list(fit, model_fit, plot_fit))
      }
    } else if (model == "tps") {
      if (length(eliminated$value) == 3) {
        # Tps cannot run when n = 3. Here, we set it to 3 because above we have already dealt with when n<=2, which results in applying a lm.
        fit <- interpolate_time_natural(eliminated)

        model_fit <- fit[[2]]
        fit <- fit[[1]]

        # Now, plot the results. Save as a ggplot object and will return with the rest of the data and models.
        plot_fit <-
          interpolate_time_plots(fit,
                                 eliminated,
                                 agemin_rounded,
                                 agemax_rounded,
                                 std.err = FALSE)

      } else {
        if (length(eliminated$value) > 3 & length(eliminated$value) <= 6) {
          smooth.dim <- 4

        } else {
          smooth.dim <- ceiling(length(eliminated$value) * 0.6)
        }

        predict.points <- as.numeric(unlist(predict.points))

        model_fit <-
          fields::Tps(eliminated$date, eliminated$value, df = smooth.dim)

        fhat <- predict(model_fit, predict.points)

        SE <- fields::predictSE(model_fit, predict.points)

        fit <-
          cbind(as.data.frame(predict.points),
                as.data.frame(fhat),
                as.data.frame(SE))
        names(fit) <- c("date", "value", "se.fit")

        # Now, plot the results. Save as a ggplot object and will return with the rest of the data and models.
        plot_fit <-
          interpolate_time_plots(fit,
                                 eliminated,
                                 agemin_rounded,
                                 agemax_rounded,
                                 std.err = TRUE)

      }

      return(list(fit, model_fit, plot_fit))

    } else if (model == "natural") {
      fit <- interpolate_time_natural(eliminated)

      model_fit <- fit[[2]]
      fit <- fit[[1]]

      # Now, plot the results. Save as a ggplot object and will return with the rest of the data and models.
      plot_fit <-
        interpolate_time_plots(fit, eliminated, agemin_rounded, agemax_rounded, std.err = FALSE)

      return(list(fit, model_fit, plot_fit))

    } else {
      stop("Please choose a valid model.")
    }

  }
