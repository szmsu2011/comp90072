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
      panel.grid.minor = element_blank()
    ) +
    labs(x = "Time (s)", y = "Amplitude (mV)")
  return(p)
}
