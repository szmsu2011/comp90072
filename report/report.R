## ---- ecg-noise
a01 <- read_ecg("../data-bin/a01.dat")
a02 <- read_ecg("../data-bin/a02.dat")
plot(a02, 198000:201000) + plot(frequency(a01))

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
