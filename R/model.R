## Down-sampler for class "ecg_hr" and "resp_ts"
down_sample <- function(x, freq = 100, target_freq = 1) {
  stopifnot(inherits(x, c("ecg_hr", "resp_ts")))
  if (inherits(x, "resp_ts")) {
    x_ds <- map(x, function(x) {
      x[seq(1, length(x), freq %/% target_freq)]
    })
  } else {
    x_ds <- x[seq(1, length(x), freq %/% target_freq)]
  }
  class(x_ds) <- class(x)
  return(x_ds)
}

## Helper function to count number of BPM using LZC
bpm <- function(x, f) {
  lzc <- x - lowess(x, f = f)$y
  est_bpm <- sum(diff(lzc > 0) != 0, na.rm = TRUE) / 2
  return(est_bpm)
}

## Generate data set for BR vs HR
brhr_dataset <- function(ecg_hr, resp_ts) {
  stopifnot(inherits(ecg_hr, "ecg_hr"))
  stopifnot(inherits(resp_ts, "resp_ts"))
  resp_ts <- resp_ts |>
    head(-1) |>
    map(function(x) tail(x, -sum(is.na(ecg_hr))))
  ecg_hr <- na.omit(ecg_hr)
  n_min <- min(length(ecg_hr), length(resp_ts[[1]])) %/% 60
  ...
}
