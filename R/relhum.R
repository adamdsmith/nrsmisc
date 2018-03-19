#' Convert air and dew point temperatures (Celsius) to relative humidity
#'
#' Uses the August-Roche-Magnus formula (Lawrence 2005, Bull. Amer.
#'  Meteor. Soc. 225-233); Values of A and B from Alduchov and Eskridge
#'  (1996, J App Meteor 35:601-609)
#'
#' @param tempC numeric vector of air temperatures (in Celsius)
#' @param dewptC numeric vector of dewpoint temperatures (in Celsius)
#' @export
#' @examples
#' relhum(13, 12.3)
#' relhum(3, -0.4)
relhum <- function(tempC, dewptC) {
  A <- 17.625; B <- 234.04
  e <- exp((A * dewptC)/(B + dewptC))
  es <- exp((A * tempC)/(B + tempC))
  RH <- round(100 * (e / es))
  return(RH)
}
