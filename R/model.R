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

## Helper function to count number of breaths per minute using SSZC
bpm <- function(x, ...) {
  lzc <- x - smooth.spline(seq_along(x), x, ...)$y
  est_bpm <- sum(diff(lzc > 0) != 0, na.rm = TRUE) / 2
  return(est_bpm)
}

## Compute chest-movement BPM and HR-derived BPM
resp_dataset <- function(ecg_hr, resp_ts, opt_lambda = NULL) {
  stopifnot(inherits(ecg_hr, "ecg_hr"))
  stopifnot(inherits(resp_ts, "resp_ts"))
  resp_ts <- resp_ts$resp_c[!is.na(ecg_hr)]
  ecg_hr <- na.omit(ecg_hr)
  n_min <- min(length(ecg_hr), length(resp_ts)) %/% 60
  chest_bpm <- map_dbl(seq_len(n_min) * 60, function(t) {
    bpm(resp_ts[seq(t - 59, t)])
  })
  if (is.null(opt_lambda)) {
    lambda <- smooth.spline(seq_len(60), head(ecg_hr, 60))$lambda *
      exp(seq(-10, 10, length = 100))
    mse <- map_dbl(lambda, function(lambda) {
      hr_bpm <- map_dbl(seq_len(n_min) * 60, function(t) {
        bpm(ecg_hr[seq(t - 59, t)], lambda = lambda)
      })
      mean((chest_bpm - hr_bpm)^2)
    })
    opt_lambda <- c(opt_lambda = lambda[which.min(mse)])
    reg_plot <- tibble(lambda = log(lambda), mse = mse) |>
      ggplot(aes(lambda, mse)) +
      geom_line() +
      geom_vline(xintercept = log(opt_lambda), col = 2) +
      theme_bw() +
      labs(
        x = expression("low bias, high var" %<-% log(lambda) %->%
          "high bias, low var    "),
        y = expression(L[2] * " loss")
      )
  }
  hr_bpm <- map_dbl(seq_len(n_min) * 60, function(t) {
    bpm(ecg_hr[seq(t - 59, t)], lambda = opt_lambda)
  })
  resp_df <- tibble(breath_chest = chest_bpm, breath_ecg = hr_bpm)
  if (exists("reg_plot")) {
    attributes(resp_df)$reg_plot <- reg_plot
  }
  attributes(resp_df)$opt_lambda <- opt_lambda
  return(resp_df)
}

## Data fusion for signals from multiple subjects
fuse_data <- function(x) {
  all_x <- map(x, function(x) {
    if (inherits(x, "resp_ts")) {
      x <- as_tibble(structure(x, class = NULL))
    }
    x <- tail(x, -2)
    head(x, dim(as.matrix(x))[1] %/% 60 * 60)
  })
  if (inherits(x[[1]], "resp_ts")) {
    all_x <- as.list(list_rbind(all_x))
  } else {
    all_x <- list_c(all_x)
  }
  class(all_x) <- class(x[[1]])
  return(all_x)
}
