## ---- data
a01 <- read_ecg("../data-bin/a01.dat")
a02 <- read_ecg("../data-bin/a02.dat")

## ---- rr-interval
plot(a01) + labs(title = "10-second sample of subject a01")
plot(frequency(a01)) + labs(title = "Frequency spectrum of subject a01")
plot(find_r_peaks(a01))
plot(a02) + labs(title = "10-second sample of subject a02")
a02_r_peaks <- find_r_peaks(a02)
plot(a02_r_peaks)
plot(a02_r_peaks, 48000:51000)
plot(a02_r_peaks, 198000:201000)
plot(frequency(a02_r_peaks)) + labs(title = "a02")

## ---- resp
plot(frequency(a02_r_peaks))
plot(down_sample(frequency(a02_r_peaks)), freq = 1)
resp <- read_resp("../data-bin/a02r.dat")
plot(resp, which = "Resp C")
plot(resp, which = "Resp A")
plot(resp, which = "Resp N")
