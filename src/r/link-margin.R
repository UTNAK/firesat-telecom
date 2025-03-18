dB <- function(x) 10 * log10(x)
dBinv <- function(x) 10^(x / 10)

spaceLoss <- function(spacePathLength, frequency) {
  if (spacePathLength$unit != "metre") stop("invalid space path length unit")
  if (frequency$unit != "hertz") stop("frequency unit")
  list(value = 2 * dB(3e8 / (4 * pi * spacePathLength$value * frequency$value)), unit = "dB")
}

informationRate <- function(bitRate, encodingScheme) {
  if (bitRate$unit != "hertz") stop("invalid bit rate unit")
  penalty <- c(1, 255/223 * 2)
  names(penalty) <- c("BPSK", "BPSKRSV")
  list(value = bitRate$value / penalty[encodingScheme], unit = "hertz")
}

erfcinv <- function (x) qnorm(x / 2, lower = FALSE) / sqrt(2)
minEbOverN0 <- function(bitErrorRate, encodingScheme) {
  if (bitErrorRate$unit != "one") stop("invalid bit error rate unit")
  t <- list(
    BPSK = function(x) erfcinv(2 * x)^2,
    BPSKRSV = function(x) 1.96667 - .06667 * log10(x)
  )
  m <- match(encodingScheme, names(t))
  list(value = if (!is.na(m)) (t[[m]])(bitErrorRate$value) else NA, unit = "dB")
}

irradiance <- function(effectivePower, spaceLoss) {
  if (effectivePower$unit != "dBW") stop("invalid effective power unit")
  if (spaceLoss$unit != "dB") stop("invalid space loss unit")
  list(value = effectivePower$value + spaceLoss$value, unit = "dBW/m^2") # is this right?
}

arrayGOverT <- function(...) {
  gl <- list(...)
  if (any(gl$unit != "dB/K")) stop("invalid G/T unit")
  list(value = dB(Reduce('+', Map(f = function(g) dBinv(g$value), gl))), unit = "dB/K")
}

k_dB <- dB(1.380649e+23)
ebOverN0 <- function(gOverT, irradiance, bitRate) {
  if (gOverT$unit != "dB/K") stop("invalid G/T unit")
  if (irradiance$unit != "dBW/m^2") stop("invalid irradiance unit")
  if (bitRate$unit != "hertz") stop("invalid bit rate unit")
  list(value = irradiance$value + gOverT$value + k_dB - dB(bitRate$value), unit = "dB")
}

linkMargin <- function(ebOverN0, minEbOverN0) {
  if (ebOverN0$unit != "dB") stop("invalid Eb/N0 unit")
  if (minEbOverN0$unit != "dB") stop("invalid min Eb/N0 unit")
  list(value = ebOverN0$value - minEbOverN0$value, unit = "dB")
}