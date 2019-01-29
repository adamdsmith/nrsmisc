#' Retrieve historical METAR weather from NOAA buoy
#'
#' Retrieve historical METAR weather from NOAA buoy; see
#'  \url{http://www.ndbc.noaa.gov/}. This function is a thin wrapper
#'  around \link[rnoaa]{buoy} mainly for multiple year queries and
#'  output formatting
#'
#' @param buoy character or integer scalar of buoy from which to retrieve weather.
#'  Browse at \url{http://www.ndbc.noaa.gov/} or see \link[rnoaa:buoy]{buoy_stations}.
#' @param years integer vector of years for which data are required
#' @param out_tz time zone specification to be used for output. System-specific
#'   (see \link[base]{timezones}. Default is "GMT" (UTC; Universal Time,
#'   Coordinated).
#' @export
#' @examples
#' \dontrun{
#' wx <- get_buoy_wx("DPIA1", years = 2017, out_tz = "America/Chicago")
#' # Current year, in GMT
#' wx <- get_buoy_wx(41063)
#' }

get_buoy_wx <- function(buoy = NULL,
                        years = as.POSIXlt(Sys.Date())$year + 1900,
                        out_tz = "GMT") {

  stopifnot(!is.null(buoy))

  if (!requireNamespace("rnoaa", quietly = TRUE))
    install.packages("rnoaa", quiet = TRUE)
  if (!requireNamespace("ncdf4", quietly = TRUE))
    install.packages("ncdf4", quiet = TRUE)

  wx <- lapply(years, function(yr) {
    tmp <- rnoaa::buoy(dataset = "stdmet", buoyid = buoy, year = yr)
    has_sst <- "sea_surface_temperature" %in% names(tmp$data)
    if (has_sst)
      tmp <- mutate(tmp[["data"]], sst_c = .data$sea_surface_temperature)
    else
      tmp <- mutate(tmp[["data"]], sst_c = NA)

    tmp %>%
      mutate(datetime = lubridate::ymd_hms(.data$time)) %>%
      select(.data$datetime,
             wdir = .data$wind_dir,
             wspd_ms = .data$wind_spd,
             tmp_c = .data$air_temperature,
             dewpt_c = .data$dewpt_temperature,
             .data$sst_c) %>%
      mutate(rh = relhum(.data$tmp_c, .data$dewpt_c))
  })
  wx <- bind_rows(wx)
  attr(wx$datetime, "tzone") <- out_tz
  wx <- mutate(wx,
               date_str = as.character(lubridate::as_date(.data$datetime)),
               hour = lubridate::hour(.data$datetime))
  wx
}

