#' Find nearest ASOS weather station by geographic location (USA only)
#'
#' The returned station abbreviations can then be passed to \code{\link{get_wx}}
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param address character scalar of a street address or place name
#'  (e.g. "Mattamuskeet NWR" or "135 Phoenix Rd, Athens, GA"); overrides
#'  \code{lat} and \code{lon} if specified. See example
#' @param n_stns how many nearest stations to return?
#' @param id_only logical; return only station abbreviations or station info too?
#' @param plot logical; generate plot showing station positions relative to input
#'  location (default \code{FALSE})
#' @export
#' @examples
#' find_wx_stns(-83, 34)
#' find_wx_stns(-83, 34, id_only = FALSE)
#' \dontrun{
#' find_wx_stns(address = "Mattamuskeet NWR", id_only = FALSE, plot = TRUE)
#' }

find_wx_stns <- function(lon = NULL, lat = NULL, address = NULL,
                         n_stns = 5, id_only = TRUE, plot = FALSE) {

  if (!requireNamespace("geosphere", quietly = TRUE))
    install.packages("geosphere", quiet = TRUE)

  if (!is.null(address)) {
    if (!requireNamespace("ggmap", quietly = TRUE))
      install.packages("ggmap", quiet = TRUE)
    if (!is.character(address))
      stop('`address` must be a character scalar of a street address ',
           'or place name (e.g. "Mattamuskeet NWR" or "135 Phoenix Rd, Athens, GA")')
    ll <- suppressMessages(ggmap::geocode(address))
    lat <- ll$lat; lon <- ll$lon
  }

  stopifnot(all(c(!is.null(lat), !is.null(lon))))
  stopifnot(identical(length(lat), length(lon)))
  stns <- readRDS(system.file("extdata/wx_stations.rds", package = "nrsmisc"))
  ll <- cbind(lon, lat)
  stns$dist_km <- as.numeric(
    round(geosphere::distm(ll, cbind(stns$lon, stns$lat)) / 1000, 1))
  stns <- arrange(stns, .data$dist_km)
  out <- utils::head(stns, n_stns)

  if (plot) {
    if (!requireNamespace("ggmap", quietly = TRUE))
      install.packages("ggmap", quiet = TRUE)
    if (!requireNamespace("ggplot2", quietly = TRUE))
      install.packages("ggplot2", quiet = TRUE)
    plot_df <- bind_rows(
      select(out, id, lon, lat),
      data.frame(id = "INPUT", lon = lon, lat = lat,
                 stringsAsFactors = FALSE)
    )
    latr <- range(plot_df$lat)
    latr <- latr + c(-1, 1) * 0.5 * diff(latr)
    lonr <- range(plot_df$lon)
    lonr <- lonr + c(-1, 1) * 0.5 * diff(lonr)
    bm <- try(suppressWarnings(
                suppressMessages(
                  bm <- ggmap::get_map(location = c(lonr[1], latr[1], lonr[2], latr[2])))),
              silent = TRUE)
    if (inherits(bm, "try-error"))
      warning("Google Maps API failure; skipping plot. Maybe wait a minute and try again.")
    else {
      bm <- ggmap::ggmap(bm) +
        ggplot2::geom_point(data = plot_df[plot_df$id == "INPUT", ],
                            ggplot2::aes(x = lon, y = lat),
                            shape = 10, size = 4, color = "red") +
        ggplot2::geom_point(data = plot_df[plot_df$id != "INPUT", ],
                            ggplot2::aes(x = lon, y = lat),
                            shape = 21, size = 2, fill = "red") +
        ggplot2::geom_label(data = plot_df, ggplot2::aes(label = id, x = lon, y = lat),
                            size=2, nudge_y = diff(latr)/35,
                            label.padding = ggplot2::unit(0.15, "lines"))
      plot(bm)
    }
  }

  if (id_only) out <- out$id
  out
}

# # Function to update available ASOS stations in US
# stations <- lapply(datasets::state.abb, function(ST) {
#   riem::riem_stations(paste0(ST, "_ASOS"))
# })
# stations <- do.call("rbind", stations)
# with(stations, plot(lon,lat))
# saveRDS(stations, file = "./inst/extdata/wx_stations.rds")
