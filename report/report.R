## ---- data
data <- read_csv("../data/learning-set/a01.csv")
plot_ecg(data) +
  labs(title = "10-second sample of subject a01")

## ---- rr-interval
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a01")
data <- read_csv("../data/learning-set/a02.csv")
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a02")
plot_ecg(data[48000:51000, ], 3000) +
  labs(title = "30-second sample of subject a02")
plot_ecg(data[180000:183000, ], 3000) +
  labs(title = "Another 30-second sample of subject a02")
plot_ecg(data[198000:201000, ], 3000) +
  labs(title = "Yet another 30-second sample of subject a02")
data <- read_csv("../data/learning-set/a03.csv")
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a03")
data <- read_csv("../data/learning-set/a04.csv")
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a04")
data <- read_csv("../data/learning-set/a05.csv")
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a05")
