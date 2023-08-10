## 5-20 Hz filter with fast fourier transform
noise_filter <- function(x) {
  x_fft <- fft(x)
  i <- seq_along(x)
  n <- length(x)
  freq_filter <- between(i, n * 0.2, n * 0.8) |
    (i > n * 0.95 | i < n * 0.05)
  x_fft[freq_filter] <- 0 + 0i
  x_filtered <- Re(fft(x_fft, inverse = TRUE)) / length(x)
  return(x_filtered)
}

## Identify the position of R-peaks in the series
r_peak_pos <- function(voltage) {
  signal <- noise_filter(voltage)
  threshold <- c(rep(Inf, 149), rollmax(signal, 150) / 2)
  lgl_1 <- signal > pmax(threshold, unname(quantile(signal, 0.95)))
  lgl_2 <- c(FALSE, diff(diff(signal) > 0) == -1, FALSE)
  r_peak <- which(lgl_1 & lgl_2)
  r_peak <- r_peak[c(FALSE, diff(r_peak) > 50)]
  return(r_peak)
}
