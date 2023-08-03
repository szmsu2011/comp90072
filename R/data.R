## Read a specified ECG data file into an integer vector
read_ecg <- function(file_name, offset = 0L, events = Inf) {
  vec_size <- file.size(file_name) / 2 # Determine data length
  events <- min(events, vec_size)
  A <- file(file_name, "rb")
  seek(A, offset * 2)
  y <- readBin(A, "integer", events, size = 2, signed = TRUE, endian = "little")
  close(A)
  return(y)
}

## Create directories to save processed ECG data files
create_data_dir <- function() {
  list("data", "data/learning-set", "data/test-set") |>
    map(function(dir) if (!dir.exists(dir)) dir.create(dir))
  invisible(0)
}

## Read all ECG data files in the repository into CSV files
process_all_ecg_files <- function(dir_name = "data-raw", ..., n_core = 1L) {
  system.time({
    ## Setup parallel processing if n_core > 1
    if (n_core > 1L && parallelly::supportsMulticore()) {
      n_core <- min(n_core, parallelly::availableCores())
      future::plan(future::multicore, workers = n_core)
    } else {
      future::plan(future::sequential)
    }
    ## Process learning set data (a01-a20, b01-b05, c01-c10)
    list.files(dir_name) |>
      stringr::str_subset("^[a-c]\\d\\d\\.dat$") |>
      furrr::future_map(function(file) {
        y <- read_ecg(sprintf("%s/%s", dir_name, file), ...)
        df <- tibble::tibble(time = seq_along(y), voltage = y)
        save_as <- sprintf("data/learning-set/%s", file) |>
          stringr::str_replace("\\.dat$", ".csv")
        readr::write_csv(df, save_as)
      })
    ## Process test set data (x01-x35)
    list.files(dir_name) |>
      stringr::str_subset("^x\\d\\d\\.dat$") |>
      furrr::future_map(function(file) {
        y <- read_ecg(sprintf("%s/%s", dir_name, file), ...)
        df <- tibble::tibble(time = seq_along(y), voltage = y)
        save_as <- sprintf("data/test-set/%s", file) |>
          stringr::str_replace("\\.dat$", ".csv")
        readr::write_csv(df, save_as)
      })
  })
}
