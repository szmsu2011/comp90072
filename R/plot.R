## Plot an ECG as a time series of amplitude (in mV)
## S3 method for class "ecg_ts"
plot.ecg_ts <- function(x, events = seq_len(10 * freq),
                        freq = 100, resolution = 200) {
  data <- tibble(
    time = seq_along(x) / freq,
    voltage = x / resolution
  ) |>
    slice(events)
  p <- data |>
    ggplot(aes(time, voltage)) +
    geom_line() +
    theme_bw() +
    labs(x = "Time (s)", y = "Amplitude (mV)")
  return(p)
}

## Plot the frequency spectrum of the ECG data
## S3 method for class "ecg_freq"
plot.ecg_freq <- function(x) {
  p <- x |>
    ggplot(aes(freq, power)) +
    geom_segment(aes(xend = freq, yend = 0), col = "steelblue") +
    labs(x = "Frequency (Hz)", y = "Relative amplitude") +
    theme_bw() +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(p)
}

## Plot the ECG with marked R-peaks
## S3 method for class "ecg_rts"
plot.ecg_rts <- function(x, events = seq(3 * freq, 23 * freq),
                         freq = 100, resolution = 200) {
  p <- plot.ecg_ts(x$ecg, events, freq, resolution)
  rp_data <- tibble(
    time = seq_along(x$ecg) / freq,
    r_peak = (seq_along(x$ecg) %in% x$r_peak) * x$ecg / resolution,
    type = "Estimated R-peaks"
  ) |>
    slice(events) |>
    filter(r_peak > 0)
  p <- p +
    geom_point(aes(y = r_peak, col = type), data = rp_data) +
    theme(legend.title = element_blank(), legend.position = "top")
  return(p)
}

## Plot the respiratory data
## S3 method for class "resp_ts"
plot.resp_ts <- function(x, events = seq_len(60 * freq),
                         freq = 100, resolution = 1,
                         which = c("all", "Resp C", "Resp A", "Resp N")) {
  which <- arg_match(which)
  p <- map2(x, seq_len(length(x)), function(ts, i) {
    plot.ecg_ts(ts, events, freq, resolution) +
      labs(
        x = ifelse(i == length(x), "Time (s)", ""),
        y = c("Resp C", "Resp A", "Resp N", "SpO2")[i]
      )
  }) |>
    set_names(c("Resp C", "Resp A", "Resp N", "SpO2"))
  if (which == "all") {
    p <- inject(wrap_plots(!!!p, ncol = 1L))
  } else {
    p <- p[[which]] + labs(x = "Time (s)")
  }
  return(p)
}

## Plot the heart rate
## S3 method for class "ecg_hr"
plot.ecg_hr <- function(x, events = seq(5 * freq, 65 * freq), freq = 100) {
  p <- plot.ecg_ts(x, events, freq, 1) +
    labs(y = "Instantaneous heart rate (BPM)") +
    geom_smooth(
      formula = y ~ s(x, k = round(diff(range(events)) / freq / 2)),
      method = "gam", linewidth = 0.65
    )
  return(p)
}
