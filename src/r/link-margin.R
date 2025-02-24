dB <- function(x) 10 * log10(x)
dBinv <- function(x) 10^(x / 10)

spaceLoss <- function(spacePathLength, frequency) {
  2 * dB(3e8 / (4 * pi * spacePathLength * frequency))
}