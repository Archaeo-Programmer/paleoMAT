interpolate_time <-
  function(temp_data) {

    Q <- quantile(temp_data$anom, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(temp_data$anom)
    up <-  Q[2]+1.5*iqr # Upper Range
    low<- Q[1]-1.5*iqr # Lower Range
    eliminated <- subset(temp_data, temp_data$anom > (Q[1] - 1.5*iqr) & temp_data$anom < (Q[2]+1.5*iqr))

    fit <- spline(eliminated$date, eliminated$anom, method = "natural")
    fit <- as.data.frame(fit)

    Q <- quantile(fit$y, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(fit$y)
    up <-  Q[2]+1.5*iqr # Upper Range
    low<- Q[1]-1.5*iqr # Lower Range
    eliminated <- subset(fit, fit$y > (Q[1] - 1.5*iqr) & fit$y < (Q[2]+1.5*iqr))

    fit <- smooth.spline(eliminated, all.knots = TRUE)

    agemax <- max(eliminated$x)
    agemax_rounded <- agemax %>%
      DescTools::RoundTo(multiple = 100, FUN = ceiling)
    agemin <- min(eliminated$x)
    agemin_rounded <- agemin %>%
      DescTools::RoundTo(multiple = 100, FUN = floor)

    midpoints <- seq((agemin_rounded + 50), (agemax_rounded - 50), by = 100)
    fit <- as.data.frame(predict(fit, agemin:agemax)) %>%
      dplyr::filter(x >= -1000 & x <= 1799) %>%
      group_by(mean = (row_number() -1) %/% 100) %>%
      mutate(mean = mean(y)) %>%
      dplyr::filter(x %in% midpoints) %>%
      dplyr::select(-y)

    return(fit)

  }
