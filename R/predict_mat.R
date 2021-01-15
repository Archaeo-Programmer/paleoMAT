# object is the mat model that was created from the modern pollen and modern climate.
predict_mat <- function (object, newdata = NULL, k = object$k, sse = FALSE,
          nboot = 100, match.data = TRUE, verbose = TRUE, lean = TRUE,
          ...)
{
  # Check to make sure that k is greater than 1 or less than or equal to k used in the modern mat model.
  if (k < 1 | k > object$k)
    stop("k out of range")
  # If the fossil pollen data is null (newdata), then return the prediction based on the number of k values;
  # just for the modern samples.
  if (is.null(newdata)) {
    return(object$fitted.values[, c(k, k + object$k), drop = FALSE])
  }
  # match.data will match two species datasets if their column names are the same (a logical).
  if (match.data) {
    # Combine the modern pollen samples (as proportions) with the fossil pollen samples into
    # a list of 2 dataframes.
    d <- rioja:::Merge(object$y, newdata, split = TRUE)
    #
    y1 <- as.matrix(d[[1]])
    y2 <- as.matrix(d[[2]])
    rownames(y2) <- rownames(newdata)
  } else {
    # The number of taxa (i.e., the # of columns) in the modern pollen and fossil pollen
    # dataset must be the same. If not, then will get an error here. Checking by # of columns and column names.
    if (ncol(object$y) != ncol(newdata))
      stop("Number of taxa does not match between datasets")
    if (any(colnames(object$y) != colnames(newdata)))
      stop("Taxon names do not match between datasets")
    # Assign modern pollen proportions to y1. Assign fossil pollen portions to y2.
    y1 <- object$y
    y2 <- newdata
  }

  # n is the number of training sets. If there are rownames (ID numbers) that overlap, then there would
  # be 2 training sets. If not, then there is 1 training set.
  if (nrow(y1) != nrow(y2)) {
    n = 1
  } else if (any(rownames(y1) == rownames(y2))) {
    n = 2
  } else {
    n = 1
  }

  # x is the environmental variable used in the model (i.e., gdd from the modern samples).
  x1 <- object$x

  # All row-wise dissimilarities between the two datasets (i.e., modern and fossil pollen).
  diss <- rioja:::paldist2(y1, y2, dist.method = object$dist.method)

  # Put the dissimilarities in order for each column, which creates an index # in each column of the dataframe.
  ind <- apply(diss, 2, order)

  # Sort the dissimilarities in each column, then only keep the top k rows and drop any FALSE, then transpose.
  # original code for next 3 lines:  dist.n <- t(apply(diss, 2, sort)[n:(n + k - 1), , drop = FALSE])
  sor <- apply(diss, 2, sort)
  sor2 <- do.call(rbind, sor)
  dist.n <- t(sor2[n:(n + k - 1), , drop = FALSE])


  rownames(dist.n) <- rownames(y2)
  colnames(dist.n) <- paste("N", sprintf("%02d", 1:k), sep = "")
  x.n <- t(matrix(x1[ind[n:(n + k - 1), , drop = FALSE]],
                  nrow = k))
  rownames(x.n) <- rownames(y2)
  colnames(x.n) <- paste("N", sprintf("%02d", 1:k), sep = "")
  xHat <- apply(x.n, 1, mean, na.rm = TRUE)
  xHat.wm <- rowSums(x.n/dist.n, na.rm = TRUE)/rowSums(1/dist.n)
  xHat <- cbind(MAT = xHat, MAT.wm = xHat.wm)
  rownames(xHat) <- rownames(y2)
  sd <- apply(x.n, 1, sd, na.rm = TRUE)
  minD <- apply(dist.n, 1, min, na.rm = TRUE)
  diagn <- data.frame(Stdev = sd, minD = minD)
  nms <- t(matrix(rownames(y1)[ind[n:(n + k - 1), , drop = FALSE]],
                  nrow = k))
  rownames(nms) <- rownames(y2)
  colnames(nms) <- paste("N", sprintf("%02d", 1:k), sep = "")
  rownames(diss) <- rownames(y1)
  colnames(diss) <- rownames(y2)
  result <- list(k = k, fit = xHat, diagnostics = diagn, dist.n = dist.n,
                 x.n = x.n, match.name = nms)
  if (!lean)
    result <- c(result, list(dist = diss))
  if (sse) {
    feedback <- ifelse(is.logical(verbose), 50, as.integer(verbose))
    if (is.null(object$dist))
      stop("No distances: refit original model using \"lean=FALSE\"")
    nsam <- nrow(object$y)
    nsam.new <- nrow(newdata)
    nest <- 2
    res2 <- array(dim = c(nsam, nest, nboot))
    res2.new <- array(dim = c(nsam.new, nest, nboot))
    for (i in 1:nboot) {
      o <- sample(nsam, replace = TRUE)
      out <- (1:nsam)[-unique(o)]
      x <- object$x[o]
      diss.m <- object$dist[o, out]
      ind <- apply(diss.m, 2, order)
      dist.n <- t(apply(diss.m, 2, sort)[1:k, , drop = FALSE])
      x.n <- t(matrix(x[ind[1:k, , drop = FALSE]], nrow = k))
      res2[out, 1, i] <- apply(x.n, 1, mean, na.rm = TRUE)
      res2[out, 2, i] <- rowSums((x.n/dist.n), na.rm = TRUE)/rowSums(1/dist.n)
      diss.f <- diss[o, ]
      ind <- apply(diss.f, 2, order)
      dist.n <- apply(diss.f, 2, sort)[1:k, , drop = FALSE]
      x.n <- matrix(x[ind[1:k, , drop = FALSE]], nrow = k)
      res2.new[, 1, i] <- apply(x.n, 2, mean, na.rm = TRUE)
      res2.new[, 2, i] <- colSums((x.n/dist.n), na.rm = TRUE)/colSums(1/dist.n)
      if (verbose) {
        if (i%%feedback == 0) {
          cat(paste("Bootstrap sample", i, "\n"))
          flush.console()
        }
      }
    }
    xHat.boot <- apply(res2, c(1, 2), mean, na.rm = TRUE)
    xHat.new.boot <- apply(res2.new, c(1, 2), mean, na.rm = TRUE)
    colnames(xHat.new.boot) <- colnames(result$fit)
    rownames(xHat.new.boot) <- rownames(newdata)
    v1.boot <- apply(res2.new, c(1, 2), sd, na.rm = TRUE)
    v2.boot <- apply(object$x - xHat.boot, 2, .rmse)
    colnames(v1.boot) <- colnames(result$fit)
    rownames(v1.boot) <- rownames(newdata)
    SEP.boot <- sqrt(sweep(v1.boot^2, 2, v2.boot^2, "+"))
    colnames(SEP.boot) <- colnames(result$fit)
    rownames(SEP.boot) <- rownames(newdata)
    result <- c(result, list(fit.boot = xHat.new.boot, v1.boot = v1.boot,
                             v2.boot = v2.boot, SEP.boot = SEP.boot))
  }
  result
}
