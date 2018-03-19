#' Add systematic empty elements to a vector
#'
#' Allows every *n*th element of a vector `x`` to either be (1)
#'  replaced with an empty character placeholder (`empty = TRUE`;
#'  default) or (2) omitted from the vector (`empty = FALSE``).
#'  Additionally, it provides the option of requesting the inverse
#'  (`inverse = TRUE`; not default) of the vector and associated
#'  empty or omitted elements. See examples.
#'
#' @param x numeric vector of air temperatures (in Celsius)
#' @param nth numeric vector of dewpoint temperatures (in Celsius)
#' @param empty logical; replace `nth` elements with empty character
#'  (`TRUE`; default) or drop from vector (`FALSE`)?
#' @param inverse logical; return the inverse of `x` after elements
#'  are set to `empty` or omitted?
#' @export
#' @examples
#' numvec <- 0:20
#' charvec <- LETTERS
#'
#' ## Replace every 3rd element with an empty character
#' every_nth(numvec, 3) # conversion to character vector
#' every_nth(charvec, 3)
#'
#' ## Omit (drop) every 3rd element
#' every_nth(numvec, 3, empty = FALSE) # vector mode is preserved
#' every_nth(charvec, 3, empty = FALSE)
#'
#' ## For creation of minor ticks, return the inverse
#'
#' ## Retain every 3rd element, replacing all others with empty character
#' every_nth(numvec, 3, inverse = TRUE) # conversion to character vector
#' every_nth(charvec, 3, inverse = TRUE)
#'
#' ## Retain every 3rd element, omitting (dropping) all other elements
#' every_nth(numvec, 3, empty = FALSE, inverse = TRUE) # vector mode is preserved
#' every_nth(charvec, 3, empty = FALSE, inverse = TRUE)
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' ## default axis labelling
#' p <- ggplot(df, aes(x, y)) + geom_point() + theme_bw()
#' p
#'
#' ## Add minor ticks to axes
#' custom_breaks <- seq(-3, 3, 0.25)
#' p +
#'   scale_x_continuous(breaks = custom_breaks,
#'   labels = every_nth(custom_breaks, 4, inverse = TRUE)) +
#'   scale_y_continuous(breaks = custom_breaks,
#'   labels = every_nth(custom_breaks, 2, inverse = TRUE))
#' }

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
