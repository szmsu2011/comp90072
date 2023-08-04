## ---- data
data <- read_csv("../data/learning-set/a01.csv")
plot_ecg(data) +
  labs(title = "10-second sample of subject a01")

## ---- rr-interval
plot_rr(data) +
  labs(title = "8-hour R-R intervals of subject a01")

## ---- abnormal-interval
interval <- rr_interval(data$voltage)
range(interval) ## Range of R-R intervals
60 / range(interval) ## Range of instantaneous heart rate per minute
abnormal_interval <- abnormal_rr(data$voltage, max_bpm = 130, min_bpm = 40)
length(abnormal_interval) ## Number of abnormal R-R intervals
length(interval) ## Total number of heart beats
invisible(map(seq_len(58), function(i) print(plot_abnormal_rr(data, i))))
