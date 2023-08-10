## ---- data
data <- read_csv("../data/learning-set/a01.csv")
plot_ecg(data) +
  labs(title = "10-second sample of subject a01")

## ---- rr-interval
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a01")
read_csv("../data/learning-set/a02.csv") |>
  plot_rr() +
  labs(title = "8-hour R-R intervals of subject a02")
read_csv("../data/learning-set/a03.csv") |>
  plot_rr() +
  labs(title = "8-hour R-R intervals of subject a03")
read_csv("../data/learning-set/a04.csv") |>
  plot_rr() +
  labs(title = "8-hour R-R intervals of subject a04")
