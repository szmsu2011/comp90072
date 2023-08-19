import numpy as np

def pulse(mu, sigma, bins):
  p = (1 / (sigma * np.sqrt(2 * np.pi)) *
    np.exp(- (bins - mu) ** 2 / (2 * sigma ** 2)))
  return p

def qrs(amp, pos, bins):
  A1 = amp
  width = 1.5
  noise = 0.005
  A2 = -A1 / 2
  AQ = A1 / 2
  AS = A1 / 2

  R1 = A1 * pulse(pos, width, bins)
  R2 = A2 * pulse(pos + width, 2 * width, bins)
  Q = AQ * pulse(pos - 10 * width, 4 * width, bins)
  S = AS * pulse(pos + 10 * width, 4 * width, bins)
  n = np.random.normal(A1 * noise, 0.5, np.size(bins))
  w = R1 + R2 + Q + S + n

  return w

bins = np.array(range(1, 300))

ECG = np.zeros(2000)
for pos in range(100, 1800, 200):
  w = qrs(400, 100, bins)
  len_w = np.size(w)
  ECG[pos:pos + len_w] = w
