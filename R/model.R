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
