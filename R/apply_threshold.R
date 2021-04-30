apply_threshold <-
  function(x, cutoff = 500, k = x$k) {
    # For the dissimilarities of the k closest analogues, change the values that are greater than the threshold (i.e., cutoff) to NA.
    # x$dist.n has the distance for k analogues at each location and depth.
    x$dist.n[x$dist.n > cutoff] <- NA
    # Create a logical matrix that shows where the NAs are in the matrix.
    l <- is.na(x$dist.n)
    # Then, replace reconstructed values of the k closest analogues (x.n) with NA, where there is an NA in dist.n.
    x$x.n <- replace(x$x.n, as.matrix(l), NA)
    # Do the same for match.name.
    x$match.name <- replace(x$match.name, as.matrix(l), NA)

    # Get number of analogues (i.e., analogues less than the cutoff) for each row.
    N <- apply(x$x.n, 1, function(z)
      length(na.omit(z)))
    # Replace any rows that have less than 2 analogues with NAs, as these samples will not be used in the reconstruction.
    x$x.n <- replace(x$x.n, N < 2, NA)
    x$dist.n <- replace(x$dist.n, N < 2, NA)
    x$match.name <- replace(x$match.name, N < 2, NA)

    # Recalculate fit (i.e., the mean of the remaining analogues).
    x$fit <- rowMeans(x$x.n, na.rm = TRUE) # I also need to add mat.wm here to just the regular MAT.

    # Get the sum of the environmental values of the k closest analogues (& that are less than the threshold) for each row.
    s <- apply(d * x$x.n, 1, sum, na.rm = TRUE)
    # Get average of the environmental values of the k closest analogues (& that are less than the threshold) for each row, and the number of analogues for each row.
    res <- data.frame(fit = s / N, N = N)
    res
  }

# Things to recalculate after applying the threshold.
# k = k
# fit = mostly the existing function above
# diagnostics = stdev of the new analogues and the min dist.n
# dist.n = distances of remaining analogues
# x.n = reconstructed values of remaining analogues
# match.name = column names of the k closest analogues
# fit.boot = mean of the bootstrap estimates of newdata.
# v1	= standard error of the bootstrap estimates for each new sample.
# v2	= root mean squared error for the training set samples, across all bootstrap samples.
# SEP	= standard error of prediction, calculated as the square root of v1^2 + v2^2.
