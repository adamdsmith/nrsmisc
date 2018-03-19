#' Calculate moon exposure for a given time period
#'
#' This function calculates the proportion of the moon's face that is illuminated,
#' and whether the moon is above horizon at a given location, for a user-specified
#' number of hours from a provided \link[base]{POSIXct} object. Specifically, it
#' sums the proportion illuminated at the midpoint of every hour, conditional on
#' the moon being above local horizon. Thus, the returned value ranges from zero
#' (new moon or moon not above horizon) to \code{exp_hrs} (full moon above horizon
#' for the entire time from \code{datetime} to \code{datetime} + \code{exp_hrs}).
#'
#' @param datetime \link[base]{POSIXct} vector of datetimes from which moon
#'   exposure should be calculated
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param exp_hrs integer of number of hours post-\code{datetime} to calculate
#'   exposure
#' @export
#' @examples
#' get_moon(as.POSIXct(Sys.time()), -83, 34, 12)
#' # For fun, plot daily moon exposure for next lunar cycle
#' next_cycle <- as.POSIXct(seq.Date(Sys.Date(), by = 1, length.out = 28))
#' plot(next_cycle, get_moon(next_cycle, -83, 34, 24))

get_moon <- function (datetime, lon, lat, exp_hrs = 6) {

  if (!requireNamespace("oce", quietly = TRUE))
    install.packages("oce", quiet = TRUE)

  stopifnot(inherits(datetime, "POSIXct"))
  attributes(datetime)$tzone <- "UTC"

  exposure <- sapply(datetime, function(dt) {
    sequence <- dt + as.difftime(seq(0.5, exp_hrs - 0.5, by = 1), units = "hours")
    moon <- oce::moonAngle(sequence, lon, lat) %>% as.data.frame() %>%
      mutate(illum = ifelse(.data$altitude < 0, 0, .data$illuminatedFraction))
    exposure <- sum(moon$illum)
  })
  return(round(exposure, 2))
}
