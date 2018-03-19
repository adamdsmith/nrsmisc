#' Find nearest ASOS weather station by geographic location (USA only)
#'
#' The returned station abbreviations can then be passed to \code{\link{get_wx}}
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @param lat numeric scalar of position latitude (decimal degrees; WGS84)
#' @param n_stns how many nearest stations to return?
#' @param id_only logical; return only station abbreviations or station info too?
#' @param plot logical; generate plot showing station positions relative to input
#'  location (default \code{FALSE})
#' @export
#' @examples
#' find_wx_stns(-83, 34)
#' find_wx_stns(-83, 34, id_only = FALSE)

find_wx_stns <- function(lon, lat, n_stns = 5, id_only = TRUE, plot = FALSE) {

  if (!requireNamespace("geosphere", quietly = TRUE))
    install.packages("geosphere", quiet = TRUE)

  stopifnot(all(c(!is.na(lat), !is.na(lon))))
  stopifnot(identical(length(lat), length(lon)))
  stns <- readRDS(system.file("extdata/wx_stations.rds", package = "adsmisc"))
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
    suppressMessages(suppressWarnings(
      bm <- ggmap::get_map(location = c(lonr[1], latr[1], lonr[2], latr[2]))
    ))
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
