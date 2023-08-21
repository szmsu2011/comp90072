## Left-align two vectors of different lengths
sync_vec <- function(x, y) {
  stopifnot(is_vector(x) && is_vector(y))
  y <- head(y, length(x))
  x <- head(x, length(y))
  n_na <- max(sum(is.na(x)), sum(is.na(y)))
  y <- tail(y, -n_na)
  x <- tail(x, -n_na)
  vecs <- list(x = x, y = y)
  return(vecs)
}

## Fit a regularised polynomial regression for Resp C ~ HR
## S3 generic object "cecg_ridge"
chest_ecg_ridge_fit <- function(y, x, lambda) {
  stopifnot(is.matrix(x) && is_vector(y))
  x <- cbind(1, x)
  z <- solve(t(x) %*% x + diag(c(0, rep(lambda, ncol(x) - 1)))) %*% t(x)
  df <- model_df(x, z)
  b <- z %*% y
  fit <- c(df = df, b = b)
  class(fit) <- c("cecg_ridge", class(fit))
  return(fit)
}

## Validation for finding best shrinkage coefficient `lambda`
validate_chest_ecg <- function(y, x, seed = 2023) {
  stopifnot(is_vector(x) && is_vector(y))
  set.seed(seed)
  data <- sync_vec(x, y)
  x <- data$x
  y <- data$y
  y <- (y - mean(y)) / sd(y)
  x <- poly(x, 20, simple = TRUE) |>
    apply(2, \(x) (x - mean(x)) / sd(x))
  vi <- sample(seq_along(y), round(length(y) * 0.8))
  lambda <- exp(seq(0, 20, length = 20)) - 1
  fits_stats <- inject(rbind(!!!map(lambda, function(l) {
    fit <- chest_ecg_ridge_fit(y[-vi], x[-vi, ], l)
    mse <- mean((y[vi] - cbind(1, x[vi, ]) %*% fit[-1])^2)
    c(fit["df"], mse = mse, lambda = l)
  })))
  return(fits_stats)
}
