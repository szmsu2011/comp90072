## ---- data
read_csv("../data/learning-set/a01.csv") |>
  plot_ecg() +
  labs(title = "10-second sample of subject a01")
