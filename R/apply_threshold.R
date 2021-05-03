apply_threshold <-
  function(x, qtile = .995, k = x$k) {
    # Calculate the cutoff value.
    # First, combine all analogues into 1 list.
    cutoff <- reshape2::melt(x$dist.n) %>%
      dplyr::select(value)
    # Next, get the cutoff for where 99.5% of the data falls within.
    cutoff <-  quantile(cutoff$value, probs=qtile)
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
    # Replace any rows that have less than 2 analogues with NAs (needs to be at least 2 analogues or do not reconstruct), as these samples will not be used in the reconstruction.
    x$x.n <- replace(x$x.n, N < 2, NA)
    x$dist.n <- replace(x$dist.n, N < 2, NA)
    x$match.name <- replace(x$match.name, N < 2, NA)
    x$fit.boot <- replace(x$fit.boot, N < 2, NA)
    x$v1.boot <- replace(x$v1.boot, N < 2, NA)
    x$SEP.boot <-   replace(x$SEP.boot, N < 2, NA)

    # Recalculate fit (i.e., the mean [MAT] and the weighted mean [MAT.wm] of the remaining analogues).
    xHat <- rowMeans(x$x.n, na.rm = TRUE)
    x$fit <-
      cbind(
        MAT = xHat,
        MAT.wm = rowSums(x$x.n / x$dist.n, na.rm = TRUE) / rowSums(1 / x$dist.n, na.rm = TRUE)
      )

    # Recalculate diagnostics.
    Stdev <- apply(x$x.n, 1, sd, na.rm = TRUE)
    x$diagnostics <-
      as.data.frame(cbind(Stdev, minD = suppressWarnings(apply(
        x$dist.n, 1, min, na.rm = TRUE
      ))))
    # When using min(), if there are only NAs, then Inf or -Inf will be returned. So, here, the Inf values are converted to NAs.
    x$diagnostics <-
      x$diagnostics %>% dplyr::mutate_if(is.numeric, list( ~ na_if(., Inf))) %>%
      dplyr::mutate_if(is.numeric, list( ~ na_if(.,-Inf)))

    x
  }
