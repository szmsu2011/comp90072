## Low-pass noise filter with fast fourier transform
noise_filter <- function(x, sample_freq = 100, cutoff_freq = 20) {
  x_fft <- fft(x)
  x_fft[seq(length(x) * cutoff_freq / sample_freq, length(x))] <- 0 + 0i
  x_filtered <- Re(fft(x_fft, inverse = TRUE)) / length(x)
  return(x_filtered)
}

## Identify the position of R-peaks in the series
r_peak_pos <- function(voltage) {
  signal <- noise_filter(abs(diff(diff(noise_filter(voltage)))))
  threshold <- c(rep(quantile(signal, 0.9), 299), rollmax(signal, 300) / 2)
  lgl_1 <- signal > pmax(threshold, quantile(signal, 0.9))
  lgl_2 <- c(FALSE, diff(diff(signal) > 0) == -1, FALSE)
  return(which(lgl_1 & lgl_2))
}

## Looped R-peaks identification to avoid long fourier transform
r_peak_pos_looped <- function(voltage) {
  breaks <- floor(seq(1, length(voltage), length = 300))
  bad_breaks <- abs(diff(diff(voltage)))[pmin(breaks, length(voltage) - 2)] >
    median(abs(diff(diff(voltage)))[pmin(breaks, length(voltage) - 2)])
  while (any(bad_breaks[-length(breaks)])) {
    breaks[bad_breaks] <- pmin(breaks[bad_breaks] + 10, length(voltage))
    bad_breaks <- abs(diff(diff(voltage)))[pmin(breaks, length(voltage) - 2)] >
      median(abs(diff(diff(voltage)))[pmin(breaks, length(voltage) - 2)])
  }
  r_peaks <- numeric(0)
  for (i in seq(length(breaks) - 1)) {
    voltage_i <- voltage[seq(breaks[i], breaks[i] + diff(breaks)[i] - 1)]
    r_peaks_i <- r_peak_pos(voltage_i)
    r_peaks <- c(r_peaks, r_peaks_i + breaks[i])
  }
  return(r_peaks)
}

## Returns all the R-R intervals (in s) in a vector
rr_interval <- function(voltage) {
  return(diff(r_peak_pos_looped(voltage)) / 100)
}

## Returns 4-second time intervals of abnormal R-R in a list
abnormal_rr <- function(voltage, max_bpm = 130, min_bpm = 40) {
  r_peak <- r_peak_pos(voltage)
  rr <- rr_interval(voltage)
  abnormal_pos <- which(!between(rr, 60 / max_bpm, 60 / min_bpm))
  abnormal_time <- r_peak[abnormal_pos] |>
    map(function(time) time + c(-200, 200))
  return(abnormal_time) # Note: the unit of `abnormal_time` is (10ms)
}
