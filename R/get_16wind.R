#' Retrieve the point of a 16-wind compass rose for 0-360 degree bearings
#'
#' This function converts compass bearings into one of 16 22.5 degree wide
#'  abbreviated text directions - specifically the eight principal winds
#'  (N, NE, E, SE, S, SW, W, NW) and eight half-winds that bisect the angles
#'  between the principal winds (NNE, ENE, ESE, SSE, SSW, WSW, WNW, NNW). The
#'  eight principal winds and the eight half-winds together form the 16-wind
#'  compass rose, with each compass point at a 22 1⁄2' angle from its neighbors
#'
#' @param degrees numeric or integer vector of compass bearings, in degrees and
#'   constrained between 0 and 360 degrees
#' @export
#' @examples
#' get_16wind(90)
#' get_16wind(270)
#' get_16wind(22.5)
#' get_16wind(c(0, 45, 90, 135))

get_16wind <- function (degrees) {

  stopifnot(is.numeric(degrees))
  if (any(!between(na.omit(degrees), 0, 360)))
    stop("Bearings are expected in degrees and should be between 0 and 360.")
  sectors <- as.integer(degrees/22.5 + 0.5)
  points <- c("N","NNE","NE","ENE","E","ESE", "SE", "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
  return(points[sectors %% 16 + 1])
}
