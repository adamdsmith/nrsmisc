#' Retrieve sunrise or sunset times by geographic location
#'
#' Very thin wrapper around \link[maptools]{sunriset} mainly for the
#'  output formatting
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param start character or Date scalar of start date ("YYYY-MM-DD")
#' @param end character or Date scalar of start date ("YYYY-MM-DD")
#' @param direction any combination of "dawn", "dusk", "sunrise", or "sunset",
#'  indicating which ephemerides should be calculated. Here, dawn and dusk
#'  represent civil sunrise and civil sunset (sun 6 degrees below horizon),
#'  respectively.
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "GMT" (UTC; Universal Time,
#'   Coordinated).
#' @export
#' @examples
#' get_sun(-83, 34)
#' get_sun(-83, 34, start = "2017-12-31", end = "2017-12-31",
#'         direction = c("sunrise", "sunset"), out_tz = "America/New_York")

get_sun <- function (lon, lat, start = Sys.Date(),
                     end = start,
                     direction = c("sunrise", "sunset", "dawn", "dusk"),
                     out_tz = "GMT") {

  stopifnot(all(direction %in% c("sunrise", "sunset", "dawn", "dusk")))

  if (!requireNamespace("maptools", quietly = TRUE))
    install.packages("maptools", quiet = TRUE)

  ll <- cbind(lon, lat)
  start <- as.POSIXct(start, tz = "GMT")
  end <- as.POSIXct(end, tz = "GMT")
  sequence <- seq(from = start, to = end, by = "days")

  sun <- lapply(direction, function(d) {
    if (d %in% c("sunrise", "sunset"))
      tmp <- maptools::sunriset(ll, sequence, direction = d, POSIXct = TRUE)$time
    else
      tmp <- maptools::crepuscule(ll, sequence, solarDep = 6, direction = d, POSIXct = TRUE)$time
    if (out_tz != "GMT") {
      attributes(tmp)$tzone <- out_tz
    }
    tmp <- data.frame(date_str = as.character(sequence), tmp, stringsAsFactors = FALSE)
    names(tmp)[2] <- d
    tmp
  })
  sun <- Reduce(function(x, y) merge(x, y, by = "date_str", all = TRUE), sun)
  sun
}
