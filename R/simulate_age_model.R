#' @name simulate_age_model
#' @title Simulate Age-depth Model from Range
#'
#' @description Create an age-depth model from the minimum and maximum years.
#'
#' @param data A tibble with pollen core data.
#' @return A tibble with the reconstruction and new age-depth model.
#' @export
simulate_age_model <-  function(data) {
  for (i in 1:nrow(data)) {
    max <- data$max[i]
    min <-
      ifelse(i == 1,
             data$min[i],
             ifelse(
               data$min[i] < data$Year[i - 1] &
                 !is.na(data$Year[i - 1]),
               data$Year[i - 1],
               data$min[i]
             ))

    data$Year[i] <- sample(c(min:max), 1)
  }
  return(data)
}
