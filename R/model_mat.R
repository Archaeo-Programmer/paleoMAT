model_mat <- function (y, x, dist.method = "sq.chord", k = 5, lean = TRUE)
{
  call.fit <- as.call(list(quote(MAT), y = quote(y), x = quote(x),
                           dist.method = dist.method, k = k, lean = lean))
  x1 <- as.numeric(x)
  n <- 2
  diss <- as.matrix(paldist(y, dist.method = dist.method))
  ind <- apply(diss, 2, order)
  dist.n <- t(apply(diss, 2, sort)[n:(n + k - 1), , drop = FALSE])
  rownames(dist.n) <- rownames(y)
  colnames(dist.n) <- paste("N", sprintf("%02d", 1:k), sep = "")
  warn.dist <- FALSE
  if (any(dist.n < 1e-06)) {
    dist.n[dist.n < 1e-06] <- 1e-06
    warn.dist <- TRUE
  }
  x.n <- t(matrix(x1[ind[n:(n + k - 1), , drop = FALSE]],
                  nrow = k))
  rownames(x.n) <- rownames(y)
  colnames(x.n) <- paste("N", sprintf("%02d", 1:k), sep = "")
  xHat <- matrix(NA, nrow = nrow(y), ncol = k * 2)
  for (i in 1:k) {
    xHat[, i] <- apply(x.n[, 1:i, drop = FALSE], 1, mean,
                       na.rm = TRUE)
    xHat[, i + k] <- rowSums((x.n/dist.n)[, 1:i, drop = FALSE],
                             na.rm = TRUE)/rowSums(1/dist.n[, 1:i, drop = FALSE])
  }
  rownames(xHat) <- rownames(y)
  colnames(xHat) <- c(paste("N", sprintf("%02d", 1:k), sep = ""),
                      paste("N", sprintf("%02d", 1:k), ".wm", sep = ""))
  sd <- apply(x.n, 1, sd, na.rm = TRUE)
  minD <- apply(dist.n, 1, min, na.rm = TRUE)
  diagn <- data.frame(Stdev = sd, minD = minD)
  nms <- t(matrix(rownames(y)[ind[n:(n + k - 1), , drop = FALSE]],
                  nrow = k))
  rownames(nms) <- rownames(y)
  colnames(nms) <- paste("N", sprintf("%02d", 1:k), sep = "")
  rownames(diss) <- rownames(y)
  colnames(diss) <- rownames(y)
  call.print <- match.call()
  result <- list(call = call.fit, call.print = call.print,
                 fitted.values = xHat, diagnostics = diagn, dist.n = dist.n,
                 x.n = x.n, match.name = nms, x = x1, dist.method = dist.method,
                 k = k, y = y)
  if (!lean)
    result <- c(result, list(dist = diss))
  result$cv.summary <- list(cv.method = "none")
  class(result) <- "MAT"
  if (warn.dist) {
    warning("Inter-sample distances < 1.0E-6 found (duplicate samples?\nThese have been replaced by 1.0E-6")
  }
  result
}
