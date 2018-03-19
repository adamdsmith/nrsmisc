#' Retrieve sunrise or sunset times by geographic location
#'
#' Very thin wrapper around \link[maptools]{sunriset} mainly for the
#'  output formatting
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param start character or Date scalar of start date ("YYYY-MM-DD")
#' @param end character or Date scalar of start date ("YYYY-MM-DD")
#' @param direction one of "dawn", "dusk", "sunrise", or "sunset", indicating
#'   which ephemerides should be calculated
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "GMT" (UTC; Universal Time,
#'   Coordinated).
#' @export
#' @examples
#' get_sun(-83, 34)
#' get_sun(-83, 34, start = "2017-12-31", end = "2017-12-31",
#'         direction = "sunset", out_tz = "America/New_York")

get_sun <- function (lon, lat, start = Sys.Date(),
                     end = start, direction = c("sunrise", "sunset"),
                     out_tz = "GMT") {

  if (!requireNamespace("maptools", quietly = TRUE))
    install.packages("maptools", quiet = TRUE)

  direction <- match.arg(direction)
  ll <- cbind(lon, lat)
  start <- as.POSIXct(start, tz = "GMT")
  end <- as.POSIXct(end, tz = "GMT")
  sequence <- seq(from = start, to = end, by = "days")
  sun <- maptools::sunriset(ll, sequence, direction = direction, POSIXct = TRUE)$time
  if (out_tz != "GMT") {
    attributes(sun)$tzone <- out_tz
  }
  sun <- data.frame(date_str = as.character(sequence), sun, stringsAsFactors = FALSE)
  names(sun)[2] <- direction
  sun
}
