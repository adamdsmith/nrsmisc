#' Capitalize first or all words in string
#'
#' @param string character vector
#' @param words which words to capitalize in string: \code{all} or \code{first} only?
#' @export
#' @examples
#' Cap(c("adam d. smith", "northern parula", "title Caps please"))
#' Cap("this is a sentence.", "first")

Cap <- function(string, words = c("all", "first")) {
  words <- match.arg(words)
  isna <- is.na(string)
  string <- tolower(string)
  if (identical(words, "all")) {
    s <- strsplit(string, " ")
    s <- sapply(s, function(i) {
      paste(toupper(substring(i, 1,1)), substring(i, 2), sep="", collapse=" ")
    })
  } else {
    s <- paste0(toupper(substr(string, 1, 1)),
                substr(string, 2, nchar(string)))
  }
  s[isna] <- NA_character_
  s
}
