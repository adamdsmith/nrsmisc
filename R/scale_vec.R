#' Transform a vector of numeric values to any custom numeric scale

#' @param vec integer or numeric vector of values to be scaled
#' @param scale integer or numeric vector of length 2 indicating the
#'   lower and upper limits of the target scale
#' @export
#' @examples
#' # Convert numeric vector to the range 0-1Use FAA code
#' v <- rnorm(10)
#' scale_vec(v)
#' v <- runif(10)
#' scale_vec(v, scale = c(0, 100))

scale_vec <- function(vec, scale = c(0, 1)) {

  stopifnot(class(vec) %in% c("numeric", "integer"))
  if (length(scale) != 2)
    stop("Target scale must be specified with a vector of length two indicating\n",
         "  the lower and upper limits of target scale.")
  scale <- sort(scale)
  out <- ((scale[2] - scale[1]) * (vec - min(vec, na.rm=TRUE))) /
         (max(vec, na.rm=TRUE) - min(vec, na.rm=TRUE)) + scale[1]
  out
}
