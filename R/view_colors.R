#' Visualize hex code colors
#'
#' Helpful for visualizing one or more colors (hex code)
#'
#' For horizontal displays, the colors proceed left to right. For vertical
#'   displays, the colors proceed bottom to top.
#'
#' @param colors character vector of hex color codes
#' @param direction character scalar indicating desired plotting direction
#'   (default is horizontal). See details.
#' @export
#' @examples
#' view_colors(c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"))
#' view_colors(c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"),
#'             direction = "vertical")
view_colors <- function(colors, direction = c("horizontal", "vertical")) {
  direction <- match.arg(direction)
  if (direction == "horizontal") x <- matrix(1:length(colors), ncol = 1)
  else x <- matrix(1:length(colors), nrow = 1)
  graphics::image(x, col = colors, axes = FALSE, xlab="", ylab = "")
}
