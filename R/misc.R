sum_selected <- function (dat, cols) {
  if (length(cols) == 1) {
    res <- dat[, cols] 
  } else {
    res <- ifelse(apply(!is.na(dat[, cols]), 1, any), rowSums(dat[, cols], na.rm=T), NA)
  }
  res
}
