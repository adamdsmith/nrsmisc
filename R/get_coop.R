#' Wrapper of rnoaa::coops_search that allows retrieval of more than
#'  one API-restricted request window at a time.
#'
#' This function is particularly useful for retrieving tide/water level data.
#'  Other functions in this package (e.g., \code{\link{get_wx}} or
#'  \code{\link{get_wx_ACIS}} may be better suited for basic meteorological data.
#'  All the products and options available at \code{\link[rnoaa:coops]{coops_search}}
#'  are available here except the ability to pass additional options to Curl.
#'  We also add add optional \code{start} and \code{end} date formats to be
#'  more consistent with other functions in this package. However, calls to
#'  \code{\link[rnoaa:coops]{coops_search}} without Curl options should work
#'  seemlessly with a call to \code{get_coop}. See example. Note, however,
#'  we return only the \code{data.frame} data component returned by
#'  \code{\link[rnoaa:coops]{coops_search}}
#'
#' @param station_name (numeric) Required station name/number. See
#'  \url{https://tidesandcurrents.noaa.gov/map/} to locate station
#'  name and view what data \code{product} might be available.
#' @param begin_date (numeric) Date in yyyymmdd format. One of
#'  \code{begin_date} or \code{start} is required.
#' @param end_date (numeric) Date in yyyymmdd format. One of
#'  \code{end_date} or \code{end} is required.
#' @param product (character) Specify the data type. See Details for options.
#'  Required
#' @param datum (character) See below for Details. Required for all water
#' level products.
#' @param units (character) One of 'metric' (default) or 'english' (imperial)
#' @param time_zone (character) Time zone, one of 'gmt' (default), 'lst',
#'  or 'lst_ldt'. See details at \code{\link[rnoaa:coops]{coops_search}}.
#' @param application (character) If called within an external package,
#'  set to the name of your organization. Optional.
#' @param start character or Date scalar of start date ("YYYY-MM-DD")
#' @param end character or Date scalar of end date ("YYYY-MM-DD")
#' @param verbose (logical) print messages while processing requests?
#'
#' @details
#' Options for the \code{product} paramater. Not all available at every station.
#'  One of:
#' \itemize{
#'  \item water_level - Preliminary or verified water levels, depending on
#'  availability
#'  \item air_temperature - Air temperature as measured at the station
#'  \item water_temperature - Water temperature as measured at the station
#'  \item wind - Wind speed, direction, and gusts as measured at the station
#'  \item air_pressure - Barometric pressure as measured at the station
#'  \item air_gap - Air Gap (distance between a bridge and the water's surface)
#'  at the station
#'  \item conductivity - The water's conductivity as measured at the station
#'  \item visibility - Visibility from the station's visibility sensor. A
#'  measure of atmospheric clarity
#'  \item humidity - Relative humidity as measured at the station
#'  \item salinity - Salinity and specific gravity data for the station
#'  \item one_minute_water_level - One minute water level data for the station
#'  \item predictions - 6 minute predictions water level data for the station
#'  \item hourly_height - Verified hourly height water level data for
#'  the station
#'  \item high_low - Verified high/low water level data for the station
#'  \item daily_mean - Verified daily mean water level data for the station
#'  \item monthly_mean - Verified monthly mean water level data for the station
#'  \item datums - datums data for the stations
#'  \item currents - Currents data for currents stations
#' }
#' Options for the datum parameter. Not all available for every station.
#'  One of:
#' \itemize{
#'  \item MHHW - Mean higher high water
#'  \item MHW - Mean high water
#'  \item MTL - Mean tide level
#'  \item MSL - Mean sea level
#'  \item MLW - Mean low water
#'  \item MLLW - Mean lower low water
#'  \item NAVD - North American Vertical Datum
#'  \item STND - Station datum
#' }
#'
#' @return A \code{data.frame} with data or \code{NULL} if no data is found
#' @export
#' @examples \dontrun{
#' # Two months of water level data at Vaca Key (8723970)
#' # Fails with coops_search because query range is too long
#' coops_search(begin_date = 20140927, end_date = 20141115,
#'              station_name = 8723970, datum = "stnd",
#'              product = "water_level")
#' # Works with get_coop with exact same argument order
#' get_coop(begin_date = 20140927, end_date = 20141115,
#'          station_name = 8723970, datum = "stnd",
#'          product = "water_level")
#' }

get_coop <- function(begin_date = NULL, end_date = NULL,
                     station_name = NULL, product, datum = NULL,
                     units = "metric", time_zone = "gmt",
                     application = "rnoaa", start = NULL, end = NULL,
                     verbose = TRUE) {

  if (!requireNamespace("rnoaa", quietly = TRUE))
  install.packages("rnoaa", quiet = TRUE)
  if (!requireNamespace("lubridate", quietly = TRUE))
    install.packages("lubridate", quiet = TRUE)

  # Standardize date format across options
  if (!is.null(begin_date) && !is.null(end_date)) {
    start <- as.Date(as.character(begin_date), format = "%Y%m%d")
    end <- as.Date(as.character(end_date), format = "%Y%m%d")
  } else if (!is.null(start) && !is.null(end)) {
    start <- as.Date(start); end <- as.Date(end)
  } else stop("Missing some required date information.\n",
              "  You must specify together either:\n",
              "    'begin_date' and 'end_date' or\n",
              "    'start' and 'end'")
  req_dur <- end - start

  # check for duration longer than NOAA will return
  # sub-hourly products with 31 day max
  group1 <- c("water_level",  "air_temperature",  "water_temperature",
              "wind", "air_pressure", "air_gap", "conductivity",
              "visibility", "humidity", "salinity", "one_minute_water_level",
              "predictions", "currents")
  # hourly to sub-daily products with 1 year max
  group2 <- c("hourly_height", "high_low")
  # daily or longer products with 10 year max
  group3 <- c("daily_mean", "monthly_mean")

  max_dur <- case_when(
    product %in% group1 ~     31,
    product %in% group2 ~    366,
    product %in% group3 ~   3653,
    TRUE                ~ 365000)

  if (req_dur < max_dur) {
    begin <- as.numeric(gsub("-", "", start))
    end <- as.numeric(gsub("-", "", end))
    out <- try(rnoaa::coops_search(begin, end, station_name, product,
                                   datum, units, time_zone, application),
               silent = TRUE)
    if (inherits(out, "try-error")) {
      if (grepl("No data", attr(out, "condition")$message)) {
        warning("No data was found for that station/product/date combination.",
                call. = FALSE)
        return(NULL)
      } else stop(attr(out, "condition")$message, call. = FALSE)
    }
    out <- out$data
  } else {
    by_dur <- case_when(
      max_dur == 31 ~ "month",
      max_dur == 366 ~ "year",
      max_dur == 3653 ~ "10 years"
    )

    n_durs <- ceiling(as.numeric(req_dur / max_dur)) + 1

    # Do it in chunks of acceptable duration to the API
    dates <- seq.Date(as.Date(start), by = by_dur, length.out = n_durs)
    dates <- as.numeric(gsub("-", "", dates))
    out <- lapply(seq_along(dates), function(i) {
      if (i > 1) { # Skip first date
        start <- dates[i-1]; end <- dates[i]
        if (verbose)
          message("Processing ", by_dur, " beginning on ",
                  format(lubridate::ymd(start), format = "%d %B, %Y"))
        tmp <- try(rnoaa::coops_search(start, end, station_name,
                                       product, datum, units, time_zone,
                                       application), silent = TRUE)
        if (inherits(tmp, "try-error")) {
          if (grepl("No data", attr(tmp, "condition")$message))
            tmp <- NULL
          else
            stop(attr(tmp, "condition")$message, call. = FALSE)
        } else {
          if (identical(product, "predictions"))
            tmp <- tmp$predictions
          else
            tmp <- tmp$data %>% repair_monthly(start, end)
        }
      }
    })
    if (all(sapply(out, is.null))) {
      warning("No data was found for that station/product/date combination.",
              call. = FALSE)
      return(NULL)
    }
    out <- bind_rows(out) %>%
      filter(t >= lubridate::ymd(start),
             t <= lubridate::ymd(end),
             !duplicated(.data$t))

  }
  out
}
