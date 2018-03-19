#' Count number of consecutive \code{TRUE} logicals
#'
#' Counts the number of consecutive \code{TRUE}s in a logical vector. Restarts
#'  count of consecutive \code{TRUE}s when \code{FALSE} is encountered. Vector
#'  can either be a \code{logical} or a condition that evaluates to a
#'  \code{logical}. See examples. Based on a note from Bill Dunlap to R-devel
#'  (\url{http://tolstoy.newcastle.edu.au/R/e4/devel/08/04/1206.html})
#'
#' @param x \code{logical} vectir or a conditional statement that evaluates to
#'  a \code{logical} vector
#' @export
#' @examples
#' tf <- sample(c(TRUE, FALSE), 15, TRUE, prob = c(0.75, 0.25))
#' consecutive_T(tf)
#'
#' x <- rnorm(15, mean = -0.5)
#' consecutive_T(x < 0)
#'
#' \dontrun{
#' # Works in tidyverse pipe context
#' library(dplyr)
#' df <- tibble(x = rnorm(10, -0.25)) %>%
#'   mutate(cons_lt_zero = consecutive_T(x < 0))
#' }

consecutive_T <- function(x){
  tmp <- cumsum(x)
  tmp - cummax((!x) * tmp)
}
