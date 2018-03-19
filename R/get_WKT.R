#' Get WKT (Well Known Text) of \link[sf]{sf} object
#'
#' Not well-tested beyond POLYGON and MULTIPOLYGON objects.
#'
#' @param sf_obj object of class \link[sf]{sf}
#' @return character scalar or string of WKT text depending on number
#'   of features in \code{sf_obj}
#' @export
#' @examples
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#' get_WKT(nc[22, ])

get_WKT <- function(sf_obj) {
  wkt_txt <- sf_obj %>%
    sf::st_convex_hull() %>%
    sf::st_geometry() %>% sf::st_as_text()
  wkt_txt
}
