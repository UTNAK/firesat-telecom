dB <- function(x) 10 * log10(x)
dBinv <- function(x) 10^(x / 10)

spaceLoss <- function(spacePathLength, frequency) {
  2 * dB(3e8 / (4 * pi * spacePathLength * frequency))
}

informationRate <- function(bitRate, encodingScheme) {
  penalty <- c(1, 255/223 * 2)
  names(penalty) <- c("BPSK", "BPSKRSV")
  bitRate / penalty[encodingScheme]
}

erfcinv <- function (x) qnorm(x / 2, lower = FALSE) / sqrt(2)
minEbOverN0 <- function(bitErrorRate, encodingScheme) {
  t <- list(
    BPSK = function(x) erfcinv(2 * x)^2,
    BPSKRSV = function(x) 1.96667 - .06667 * log10(x)
  )
  m <- match(encodingScheme, names(t))
  if (!is.na(m)) (t[[m]])(bitErrorRate) else NA
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