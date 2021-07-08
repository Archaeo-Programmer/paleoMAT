interpolate_time_natural <-
  function(eliminated) {

    model_fit <-
      spline(eliminated$date, eliminated$value, method = "natural")
    model_fit <- as.data.frame(model_fit)

    Q <- quantile(model_fit$y, probs = c(.01, .99), na.rm = FALSE)
    iqr <- IQR(model_fit$y)
    up <-  Q[2] + 1.5 * iqr # Upper Range
    low <- Q[1] - 1.5 * iqr # Lower Range
    eliminated <-
      subset(model_fit, model_fit$y > (Q[1] - 1.5 * iqr) & model_fit$y < (Q[2] + 1.5 * iqr))

    model_fit <- smooth.spline(eliminated, all.knots = TRUE)

    agemax <- max(eliminated$x)
    agemax_rounded <- agemax %>%
      DescTools::RoundTo(multiple = 100, FUN = ceiling)
    agemin <- min(eliminated$x)
    agemin_rounded <- agemin %>%
      DescTools::RoundTo(multiple = 100, FUN = floor)

    midpoints <-
      seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100)
    fit <- as.data.frame(predict(model_fit, agemin:agemax)) %>%
      dplyr::filter(x >= -1000 & x <= 1799) %>%
      dplyr::group_by(mean = (row_number() - 1) %/% 100) %>%
      dplyr::mutate(mean = mean(y)) %>%
      dplyr::filter(x %in% midpoints) %>%
      dplyr::select(-y) %>%
      dplyr::rename(date = x, value = mean)

    return(list(fit, model_fit))

  }
