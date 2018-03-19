#' Retrieve UTM zone for a given position on the globe
#'
#' There are likely some places where this fails but it works for every
#' place I've tried.
#'
#' @param lon numeric scalar of position longitude (decimal degrees; WGS84)
#' @export
#' @examples
#' get_UTM_zone(-83)

get_UTM_zone <- function(lon) {
  (floor((lon + 180)/6) %% 60) + 1
}
