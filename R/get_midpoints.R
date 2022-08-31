get_midpoints <- function(x, dp = 2) {
  lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  return(round(lower + (upper - lower) / 2, dp))
}
