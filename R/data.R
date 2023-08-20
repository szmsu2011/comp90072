## Read a specified ECG data file
## S3 generic object "ecg_ts"
read_ecg <- function(file_name, events = Inf) {
  vec_size <- file.size(file_name) / 2
  events <- min(events, vec_size)
  A <- file(file_name, "rb")
  y <- readBin(A, "integer", events, size = 2, signed = TRUE, endian = "little")
  close(A)
  class(y) <- c("ecg_ts", class(y))
  return(y)
}

## Read a specified respiratory data file
## S3 generic object "resp_ts"
read_resp <- function(file_name, events = Inf) {
  vec_size <- file.size(file_name) / 2
  events <- min(events * 4, vec_size)
  A <- file(file_name, "rb")
  y <- readBin(A, "integer", events, size = 2, signed = TRUE, endian = "little")
  close(A)
  resp <- list(
    resp_c = y[seq_len(vec_size) %% 4 == 1],
    resp_a = y[seq_len(vec_size) %% 4 == 2],
    resp_n = y[seq_len(vec_size) %% 4 == 3],
    spo2 = y[seq_len(vec_size) %% 4 == 0]
  )
  class(resp) <- c("resp_ts", class(y))
  return(resp)
}
