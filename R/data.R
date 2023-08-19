## Read a specified ECG data file into an integer vector
read_ecg <- function(file_name, offset = 0L, events = Inf) {
  vec_size <- file.size(file_name) / 2
  events <- min(events, vec_size)
  A <- file(file_name, "rb")
  seek(A, offset * 2)
  y <- readBin(A, "integer", events, size = 2, signed = TRUE, endian = "little")
  close(A)
  trailing_zero <- length(y) - which(cumsum(rev(y)) == 0) + 1
  if (length(trailing_zero) > 0) y <- y[-trailing_zero]
  class(y) <- c("ecg_ts", class(y))
  return(y)
}
