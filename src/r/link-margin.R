dB <- function(x) 10 * log10(x)
dBinv <- function(x) 10^(x / 10)

spaceLoss <- function(spacePathLength, frequency) {
  2 * dB(3e8 / (4 * pi * spacePathLength * frequency))
}

informationRate <- function(bitRate) {
  bitRate
}

erfcinv <- function (x) qnorm(x / 2, lower = FALSE) / sqrt(2)
minEbOverN0 <- function(bitErrorRate) {
  erfcinv(2 * bitErrorRate)^2
}

irradiance <- function(effectivePower, spaceLoss) {
  effectivePower + spaceLoss
}

k_dB <- dB(1.380649e+23)
combine_gt <- function(l) dB(Reduce('+', Map(dBinv, l)))
ebOverN0 <- function(gOverT, irradiance, bitRate) {
  irradiance + combine_gt(gOverT) + k_dB - dB(bitRate)
}

linkMargin <- function(ebOverN0, minEbOverN0) {
  ebOverN0 - minEbOverN0
}