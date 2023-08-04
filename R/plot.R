## Plot an ECG as a time series of amplitude (in mV)
plot_ecg <- function(data, max_events = 1000) {
  data <- slice_head(data, n = max_events) |>
    mutate(time = time / 100, voltage = voltage / 200)
  p <- data |>
    ggplot(aes(time, voltage)) +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = c(0, seq_len(ceiling(max(data$time))))) +
    scale_y_continuous(breaks = c(0, seq_len(ceiling(max(data$voltage))))) +
    theme(
      panel.grid.major = element_line(colour = "pink"),
      panel.grid.minor = element_line(colour = "pink")
    ) +
    labs(x = "Time (s)", y = "Amplitude (mV)")
  return(p)
}

## Plot the R-R intervals as series of consecutive heart beats
plot_rr <- function(data, max_events = Inf) {
  rr <- rr_interval(data$voltage)
  p <- tibble(rr = rr, time = seq_along(rr)) |>
    ggplot(aes(time, rr)) +
    geom_line() +
    geom_hline(yintercept = 60 / c(40, 130), colour = "red") +
    theme_bw() +
    labs(x = "nth heart beat", y = "R-R interval (s)")
  return(p)
}

## Plot the ECG with selected interval of abnormal R-R
plot_abnormal_rr <- function(data, event = 1L, max_bpm = 130, min_bpm = 40) {
  abnormal_interval <- abnormal_rr(data$voltage, max_bpm, min_bpm)
  event <- min(event[1L], length(abnormal_interval))
  abnormal_point <- abnormal_interval[[event]]
  data <- filter(data, between(time, abnormal_point[1], abnormal_point[2]))
  recorded_rr <- rr_interval(data$voltage)
  abnormal_rr <- recorded_rr[!between(recorded_rr, 60 / max_bpm, 60 / min_bpm)]
  p <- plot_ecg(data) +
    labs(title = sprintf("Recorded abnormal R-R interval of %ss", abnormal_rr))
  return(p)
}
