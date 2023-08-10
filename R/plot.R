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
  rr <- head(diff(r_peak_pos(data$voltage)) / 100, max_events)
  p <- tibble(rr = rr, time = seq_along(rr)) |>
    ggplot(aes(time, rr)) +
    geom_line() +
    ylim(c(0, ceiling(max(rr)))) +
    theme_bw() +
    labs(x = "nth heart beat", y = "R-R interval (s)")
  return(p)
}
