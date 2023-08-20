## Return the frequency spectrum of the ECG data
## S3 method for class "ecg_ts"
frequency.ecg_ts <- function(x, freq = 100, sample_events = 1e5) {
  x_fft <- abs(fft(head(x, sample_events)))
  x_freq <- tibble(
    freq = seq(0, freq / 2, length = length(x_fft) / 2),
    power = head(x_fft, length(x_fft) / 2)
  )
  class(x_freq) <- c("ecg_freq", class(x_freq))
  return(x_freq)
}

## 5-20 Hz filter with fast fourier transform
noise_filter <- function(x) {
  stopifnot(inherits(x, "ecg_ts"))
  x_fft <- fft(x)
  i <- seq_along(x)
  n <- length(x)
  freq_filter <- between(i, n * 0.2, n * 0.8) | (i > n * 0.95 | i < n * 0.05)
  x_fft[freq_filter] <- 0 + 0i
  x_filtered <- Re(fft(x_fft, inverse = TRUE)) / length(x)
  return(x_filtered)
}

## Correct phase-shifts from fast fourier transform
correct_phase <- function(x) {
  stopifnot(inherits(x, "ecg_rts"))
  possible_shifts <- with(x, cbind(
    lag(ecg, 4), lag(ecg, 3), lag(ecg, 2), lag(ecg, 1), ecg,
    rev(lag(rev(ecg), 1)), rev(lag(rev(ecg), 2)),
    rev(lag(rev(ecg), 3)), rev(lag(rev(ecg), 4))
  ))[x$r_peak, ]
  shifts <- array_branch(possible_shifts, 1) |>
    map_dbl(which.max)
  x$r_peak <- x$r_peak + shifts - 5
  return(x)
}

## Mark the position of R-peaks in the series
find_r_peaks <- function(x) {
  stopifnot(inherits(x, "ecg_ts"))
  signal <- noise_filter(x)
  threshold <- rollmax(signal, 101, fill = Inf) / 2
  lgl_1 <- signal > pmax(threshold, unname(quantile(signal, 0.95)))
  lgl_2 <- c(FALSE, diff(diff(signal) > 0) == -1, FALSE)
  r_peak <- which(lgl_1 & lgl_2)
  r_peak <- r_peak[c(diff(r_peak) > 30, TRUE)]
  rts <- list(ecg = x, r_peak = r_peak)
  class(rts) <- "ecg_rts"
  rts <- correct_phase(rts)
  return(rts)
}
