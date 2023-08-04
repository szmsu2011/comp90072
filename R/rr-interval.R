## Identify the position of R-points in the series
r_point_pos <- function(voltage, threshold = 0.8) {
  lgl_1 <- voltage > threshold * 200
  lgl_2 <- c(FALSE, diff(diff(voltage) > 0) == -1, FALSE)
  return(which(lgl_1 & lgl_2))
}

## Returns all the R-R intervals (in s) in a vector
rr_interval <- function(voltage) {
  return(diff(r_point_pos(voltage)) / 100)
}

## Returns 4-second time intervals of abnormal R-R in a list
abnormal_rr <- function(voltage, max_bpm = 130, min_bpm = 40) {
  r_point <- r_point_pos(voltage)
  rr <- rr_interval(voltage)
  abnormal_pos <- which(!between(rr, 60 / max_bpm, 60 / min_bpm))
  abnormal_time <- r_point[abnormal_pos] |>
    map(function(time) time + c(-200, 200))
  ## Note: the unit of `abnormal_time` is (10ms)
  return(abnormal_time)
}
