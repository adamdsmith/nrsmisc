#' \code{adsmisc} package
#'
#' Miscellaneous functions for life in the natural resources science, including
#'  retrieving weather (from multiple potential sources), tides, sunrise/sunset
#'  and moon face illumination for a particular geographic location. Also includes
#'  a few random functions to read tables from Access databases, manipulate spatial
#'  data, numeric vectors, and strings, etc.
#'
#' @docType package
#' @name adsmisc
#' @import dplyr
#' @import sf
#' @importFrom rlang .data
#' @importFrom utils install.packages
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
