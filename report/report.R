## ---- ecg-noise
a01 <- read_ecg("../data-bin/a01.dat")
a02 <- read_ecg("../data-bin/a02.dat")
plot(a02, 198000:201000) + plot(frequency(a01))

## ---- ecg-process
p1 <- plot(a02, 1000:2000) +
  labs(title = "(1) Raw ECG signal")
p2 <- plot(noise_filter(a02), 1000:2000) +
  labs(title = "(2) FFT filter to suppress P and T waves")
p3 <- plot(find_r_peaks(noise_filter(a02)), 1000:2000) +
  labs(title = "(3) Find R peak on filtered signal")
p4 <- plot(find_r_peaks(a02), 1000:2000) +
  labs(title = "(4) Map R peak to raw signal with phase correction")
p5 <- plot(frequency(find_r_peaks(a02)), 1000:2000) +
  labs(title = "(5) Derive heart rate from R-R intervals")
p1 / p2 / p3 / p4 / p5

## ---- sszc
a02r <- down_sample(read_resp("../data-bin/a02r.dat"))
ph <- plot(down_sample(frequency(find_r_peaks(a02))), 5:60, freq = 1)
ph <- ph + geom_hline(yintercept = mean(ph$data$voltage), col = 4) +
  geom_line(aes(y = s), col = 2, data = tibble(
    time = ph$data$time,
    s = with(ph$data, smooth.spline(time, voltage, lambda = 1.57e-6)$y)
  ))
pr <- plot(a02r, 5:60, freq = 1, which = "Resp C")
pr <- pr + geom_hline(yintercept = mean(pr$data$voltage), col = 4) +
  geom_line(aes(y = s), col = 2, data = tibble(
    time = pr$data$time,
    s = with(pr$data, smooth.spline(time, voltage)$y)
  ))
pr / ph

## ---- resid-check
hr <- read_rds("../R/hr.rds")
resp <- read_rds("../R/resp.rds")
resp_df <- resp_dataset(hr, resp)
set.seed(2023)
resid_plot <- with(resp_df, tibble(
  f = jitter(breath_ecg),
  r = jitter(breath_chest - breath_ecg),
  s = predict(smooth.spline(f, r), f)$y
)) |>
  ggplot(aes(f, r)) +
  geom_point(shape = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), col = 2) +
  labs(x = "Fitted values", y = "Residuals") +
  theme_bw()
reg_plot <- attributes(resp_df)$reg_plot
wrap_plots(resid_plot, reg_plot, widths = c(1, 1.2))

## ---- data
training_set <- c("a01", "a02", "a03", "a04", "b01")
test_set <- c("c01", "c02", "c03")
hr <- fuse_data(map(
  sprintf("../data-bin/%s.dat", training_set),
  function(ecg_file) {
    ecg_file |>
      read_ecg() |>
      find_r_peaks() |>
      frequency() |>
      down_sample()
  }
))
resp <- fuse_data(map(
  sprintf("../data-bin/%sr.dat", training_set),
  function(resp_file) down_sample(read_resp(resp_file))
))
hr_test <- fuse_data(map(
  sprintf("../data-bin/%s.dat", test_set),
  function(ecg_file) {
    ecg_file |>
      read_ecg() |>
      find_r_peaks() |>
      frequency() |>
      down_sample()
  }
))
resp_test <- fuse_data(map(
  sprintf("../data-bin/%sr.dat", test_set),
  function(resp_file) down_sample(read_resp(resp_file))
))
write_rds(hr, "../R/hr.rds")
write_rds(resp, "../R/resp.rds")
write_rds(hr_test, "../R/hr-test.rds")
write_rds(resp_test, "../R/resp-test.rds")

## ---- train
hr <- read_rds("../R/hr.rds")
resp <- read_rds("../R/resp.rds")
resp_df <- resp_dataset(hr, resp)

## ---- result
attributes(resp_df)$opt_lambda
resp_df
summary(resp_df)

## ---- diag
c("R-F cor" = with(resp_df, cor(breath_chest - breath_ecg, breath_ecg)))
test <- lm(breath_chest ~ 0 + breath_ecg, resp_df)
c("p-value" = unname(pchisq(
  sum(resp_df$breath_ecg^2) * (coef(test) - 1)^2 *
    (nrow(resp_df) - 1) / sum(residuals(test)^2), 1,
  lower.tail = FALSE
)))
confint(test)

## ---- eval
hr_test <- read_rds("../R/hr-test.rds")
resp_test <- read_rds("../R/resp-test.rds")
resp_dt <- resp_dataset(hr_test, resp_test, 1.571851e-06)
c("RMSEP" = with(resp_dt, sqrt(mean((breath_chest - breath_ecg)^2))))
