#' Retrieve METAR weather from a US ASOS station
#'
#' Retrieve METAR weather from a US ASOS station
#'
#' A vector of dates can also be passed to the \code{start} argument if
#'  desired, in which case the \code{end} argument is ignored.
#'
#' @param stn character scalar of US ASOS station from which to retrieve
#'   weather; see \code{\link{find_wx_stns}} using option \code{asos_only} == \code{TRUE}.
#' @param start character or Date scalar OR vector (see Details) of start date ("YYYY-MM-DD")
#' @param end character or Date scalar of end date ("YYYY-MM-DD"); optional (defaults
#'   to same day as start)
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "GMT" (UTC; Universal Time,
#'   Coordinated).
#' @export
#' @examples
#' get_wx("ATL", "2017-12-25")
#' get_wx("AHN", c("2017-11-01", "2017-12-01"))

get_wx <- function(stn = NULL, start = Sys.Date(), end = start, out_tz = "GMT") {

  stopifnot(!is.null(stn))
  stns <- readRDS(system.file("extdata/wx_stations.rds", package = "nrsmisc"))
  stopifnot(stn %in% stns$id)

  if (!requireNamespace("riem", quietly = TRUE))
    install.packages("riem", quiet = TRUE)

  start <- as.Date(start); end <- as.Date(end) + as.difftime(1, units = "days")
  if (length(start) > 1) {
    if (!requireNamespace("pbapply", quietly = TRUE))
      install.packages("pbapply", quiet = TRUE)
    out <- pbapply::pblapply(start, function(dt) {
      tmp <- suppressWarnings(riem::riem_measures(stn, dt, dt))
    })
    out <- bind_rows(out)
  } else {
    out <- suppressWarnings(riem::riem_measures(stn, start, end))
  }

  if (nrow(out) == 0) stop("No results found.")
  if (!requireNamespace("weathermetrics", quietly = TRUE))
    install.packages("weathermetrics", quiet = TRUE)

  out <- out %>%
    mutate(tmp_c = weathermetrics::fahrenheit.to.celsius(.data$tmpf),
           wspd_ms = suppressWarnings(weathermetrics::convert_wind_speed(.data$sknt, "knots", "mps"))) %>%
    select(wx_stn = .data$valid, datetime = .data$valid, .data$tmp_c,
           rh = .data$relh, .data$wspd_ms, wdir = .data$drct, vis = .data$vsby,
           .data$skyc1:.data$skyc3, .data$skyl1:.data$skyl3)
  attr(out$datetime, "tzone") <- out_tz
  out

}
