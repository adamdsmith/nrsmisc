#' Retrieve meteorological data from the Applied Climate Information System (ACIS)
#'
#' SHubbard, K. G., A. T. DeGaetano, and K. D. Robbins. 2004. A modern Applied Climate
# Information System. Bull. Amer. Meteor. Soc. 85:811-812. doi:10.1175/BAMS-85-6-811.
# See also http://www.rcc-acis.org/docs_webservices.html

#' @param stn scalar of station identification code for which to retrieve
#'  meteorological data. Several options are available, such as WBAN, COOP, FAA, WMO,
#'  ICAO, and GHCN. \code{\link{find_wx_stns}} returns the FAA code. See the
#'  \code{Required parameters} documentation for the ACIS web server for details.
#'  (\url{http://www.rcc-acis.org/docs_webservices.html})
#' @param start character or Date scalar of start date ("YYYY-MM-DD")
#' @param end character or Date scalar of end date ("YYYY-MM-DD"); optional (defaults
#'   to same day as start)
#' @param elems character vector of meteorological elements to retrieve.
#'  Default is to retrieve daily maximum, minimum, and average temperatures,
#'  precipitation and snowfall totals, and snow depth. See \code{\link{ACIS_elems}} or
#'  the \code{Optional parameters} documentation for the ACIS web server for additional
#'  details and options (\url{http://www.rcc-acis.org/docs_webservices.html}). Custom
#'  cooling, heating, and growing degree days, water equivalent of snow depth, and pan
#'  evaporation not currently supported.
#' @export
#' @examples
#' # McKellar Airport, Jackson, TN
#' # Use FAA code
#' (jma_faa <- find_wx_stns(-88.824885, 35.636737, n_stns = 1))
#' jma <- get_wx_ACIS("MKL", start = "2017-04-01", end = "2017-04-30")
#' # Use GHCN code (see rnoaa::ghcn_stations())
#' jma2 <- get_wx_ACIS("USW00003811", start = "2017-04-01", end = "2017-04-30")
#' identical(jma, jma2)

get_wx_ACIS <- function(stn = NULL, start = Sys.Date(), end = start,
                        elems = c("maxt", "mint", "avgt", "pcpn", "snow", "snwd")) {

  stopifnot(!is.null(stn))
  valid_elems <- ACIS_elems()
  if (!all(elems %in% valid_elems$Code))
    stop("Unknown element code. Consult `ACIS_elems()` for valid options.")
  var_rows <- match(elems, valid_elems$Code)
  col_names <- c("date", valid_elems$VarName[var_rows])
  col_types <- paste0(c("D",
                        substr(valid_elems$VarModeAbb[var_rows], 1, 1)),
                      collapse = "")

  if (!requireNamespace("httr", quietly = TRUE))
    install.packages("httr", quiet = TRUE)
  if (!requireNamespace("readr", quietly = TRUE))
    install.packages("readr", quiet = TRUE)

  start <- as.Date(start); end <- as.Date(end)

  # Construct URL
  base_url <- paste0("http://data.rcc-acis.org/StnData?sid=", stn)
  q_url <- paste(base_url,
                 paste0("sdate=", start),
                 paste0("edate=", end),
                 paste0("elems=", paste(elems, collapse = ",")),
                 "output=csv", sep = "&")
  tmp <- httr::GET(q_url)
  httr::stop_for_status(tmp)
  tmp <- httr::content(tmp, as = "text", encoding = "UTF-8")

  # Replace trace precip, snowfall, snow depth ('T') with zeros and tidy
  tmp <- gsub("T", "0", tmp)
  out <- readr::read_csv(tmp,
                         col_names = col_names,
                         col_types = col_types,
                         na = c("", "M"),
                         skip = 1)
  out
}

#' See meteorological elements available for retrieval from \code{\link{get_wx_ACIS}}.
#'
#' Default is to retrieve daily maximum, minimum, and average temperatures,
#'  precipitation and snowfall totals, and snow depth. See also the
#'  \code{Optional parameters} documentation for the ACIS web server
#'  (\url{http://www.rcc-acis.org/docs_webservices.html}).
#' @export

ACIS_elems <- function() {
  elems <- data.frame(
    Code = c("maxt", "mint", "avgt", "obst", "pcpn", "snow", "snwd", "cdd",
             "hdd", "gdd"),
    Description = c("Maximum temperature (\u00B0F)", "Minimum temperature (\u00B0F)",
                    "Average temperature (\u00B0F)", "Obs time temperature (\u00B0F)",
                    "Precipitation (inches)", "Snowfall (inches)",
                    "Snow depth (inches)",
                    "Cooling Degree Days (above default base 65)",
                    "Heating Degree Days (below default base 65)",
                    "Growing Degree Days (above default base 50)"),
    VarName = c("tmax", "tmin", "tavg", "tobs", "precip_in", "snow_in",
                "snow_depth_in", "cdd", "hdd", "gdd"),
    VarModeAbb = c(rep("integer", 2), "number", "integer",
                   rep("number", 3),
                   rep("integer", 3)),
    stringsAsFactors = FALSE)
  elems
}
