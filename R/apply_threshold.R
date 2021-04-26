apply_threshold <-
  function(x, cutoff = 500) {
    # x$dist.n has the distance for k analogues at each location and depth. Determine which analogues are less than the cut off (as a logical).
    d <- x$dist.n < cutoff
    # Get number of trues (i.e., analogues less than the cutoff) for each row.
    N <- apply(d, 1, sum)
    # Get the sum of the environmental values of the k closest analogues (& that are less than the threshold) for each row.
    s <- apply(d * x$x.n, 1, sum, na.rm = TRUE)
    # Get average of the environmental values of the k closest analogues (& that are less than the threshold) for each row, and the number of analogues for each row.
    res <- data.frame(fit = s / N, N = N)
    res
  }
