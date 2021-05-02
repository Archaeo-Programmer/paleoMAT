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
    # Replace any rows that have less than 2 analogues with NAs (needs to be at least 2 analogues or do not reconstruct), as these samples will not be used in the reconstruction.
    x$x.n <- replace(x$x.n, N < 2, NA)
    x$dist.n <- replace(x$dist.n, N < 2, NA)
    x$match.name <- replace(x$match.name, N < 2, NA)

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
      x$diagnostics %>% mutate_if(is.numeric, list( ~ na_if(., Inf))) %>%
      mutate_if(is.numeric, list( ~ na_if(.,-Inf)))

#Recalculate fit.boot.


  }

#Testing rerunning/recalculating SSE.
nsam <- nrow(object$y)
nsam.new <- nrow(newdata)
nest <- 2
res2 <- array(dim=c(nsam, nest, nboot))
res2.new <- array(dim=c(nsam.new, nest, nboot))
#    .set.rand.seed(100)
for (i in 1:nboot) {
  #      o <- apply(data.frame(rep(nsam, nsam)), 1, .get.rand) + 1
  o <- sample(nsam, replace=TRUE)
  out <- (1:nsam)[-unique(o)]
  x <- object$x[o]

  diss.m <- object$dist[o, out]
  ind <- apply(diss.m, 2, order)
  dist.n <- t(apply(diss.m, 2, sort)[1:k, , drop=FALSE])
  x.n <- t(matrix(x[ind[1:k, , drop=FALSE]], nrow=k))
  res2[out, 1, i] <- apply(x.n, 1, mean, na.rm=TRUE)
  res2[out, 2, i] <- rowSums((x.n / dist.n), na.rm=TRUE) / rowSums(1/dist.n)

  diss.f <- diss[o, ]
  ind <- apply(diss.f, 2, order)
  dist.n <- apply(diss.f, 2, sort)[1:k, , drop=FALSE]
  x.n <- matrix(x[ind[1:k, , drop=FALSE]], nrow=k)
  res2.new[, 1, i] <- apply(x.n, 2, mean, na.rm=TRUE)
  res2.new[, 2, i] <- colSums((x.n / dist.n), na.rm=TRUE) / colSums(1/dist.n)
  if (verbose) {
    if (i %% feedback == 0) {
      cat (paste("Bootstrap sample", i, "\n"))
      flush.console()
    }
  }
}
xHat.boot <- apply(res2, c(1,2), mean, na.rm=TRUE)
xHat.new.boot <- apply(res2.new, c(1,2), mean, na.rm=TRUE)
colnames(xHat.new.boot) <- colnames(result$fit)
rownames(xHat.new.boot) <- rownames(newdata)
v1.boot <- apply(res2.new, c(1,2), sd, na.rm=TRUE)
v2.boot <- apply(object$x-xHat.boot, 2, .rmse)
colnames(v1.boot) <- colnames(result$fit)
rownames(v1.boot) <- rownames(newdata)
SEP.boot <- sqrt(sweep(v1.boot^2, 2, v2.boot^2, "+"))
colnames(SEP.boot) <- colnames(result$fit)
rownames(SEP.boot) <- rownames(newdata)
result <- c(result, list(fit.boot=xHat.new.boot, v1.boot=v1.boot, v2.boot=v2.boot, SEP.boot=SEP.boot))




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
