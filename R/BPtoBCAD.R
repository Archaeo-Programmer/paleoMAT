BPtoBCAD <- function (x)
{
  index <- !is.na(x)
  res <- matrix(c(x, rep(NA, length(x))), ncol = 2)
  res[index & x < 1950, 2] <- 1950 - res[index & x < 1950,
                                         1]
  res[index & x >= 1950, 2] <- 1949 - res[index & x >= 1950,
                                          1]
  return(res[, 2])
}
