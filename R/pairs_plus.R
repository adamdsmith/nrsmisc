#' Create matrix of scatterplots displaying pairwise correlation information,
#'  LOWESS smooth, and histogram/densityinformation
#'
#' \code{\link[graphics]{pairs}} on steroids. Intended for use as one component of
#'  exploratory data analysis
#'
#' @param x matrix or data.frame with columns (variables) for which to generate a matrix
#'  of scatterplots (like \code{\link[graphics]{pairs}}) but with additional density
#'  and correlation data to assist data exploration. NOTE that logical, character, and factor
#'  columns are converted to integers, which may or may not be useful.
#' @param cor_method character scalar indicating which correlation coefficient to compute.
#'  See \code{\link[stats]{cor}}. One of "spearman" (defaul), "pearson", or "kendall".
#' @param scale logicial (default = TRUE); scale text indicating correlation coefficient
#'  with absolute magnitude of correlation?
#' @param pch An integer specifying a symbol or a single character to be used when
#'  plotting points. See \code{\link[graphics]{points}} for possible values and their
#'  interpretation.
#' @param jitter logical (default = FALSE); should values have a small amount of noise
#'  added to aid differentiation? If TRUE, control jitter amount with \code{factor}
#'  argument. See \code{\link[base]{jitter}}
#' @param factor numeric to change amount of jitter. See details of \code{\link[base]{jitter}}
#'  for explanation.
#' @param smooth_span smoother span; the proportion of points in the plot which influence
#'  the smoothness of the fitted LOWESS line. Restricted to (0, 1) here. Default is 2/3.
#' @export
#' @examples
#' \dontrun{
#' a <- sort(runif(100))
#' df <- data.frame(super = a,
#'                  cali = a + rnorm(100, 0, 0.5),
#'                  fragi = rev(a) + rnorm(100, 0, 1),
#'                  listic = (a + rnorm(100, 0, 0.25)) ^ 2,
#'                  expi = rep(LETTERS[1:4], each = 25),
#'                  ali = a - rnorm(100, 0, 1.5),
#'                  docious = a + rnorm(100, 0, 2))
#' pairs_plus(df)
#' pairs_plus(df, scale = FALSE)
#' pairs_plus(df, smooth_span = 1/4)
#' pairs_plus(df, cor_method = "pearson")
#' }

pairs_plus <- function (x, cor_method = c("spearman", "pearson", "kendall"),
                        scale = TRUE, pch = 20, jitter = FALSE, factor = 2,
                        smooth_span = 2/3) {
  if (is.matrix(x)) x <- as.data.frame(x)
  if (!(smooth_span > 0 && smooth_span < 1)) stop("Smooth span should be in the set (0, 1).")

  # Convert strings to factors
  x <- mutate_if(x, is.character, as.factor)

  cor_method <- match.arg(cor_method)
  # Define potential summary components for panels to graphics::pairs

  "panel_hist_density" <- function(x, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(usr[1:2], 0, 1.5))
    h <- graphics::hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1], y, col = "gray50")
    tryd <- try(d <- stats::density(x, na.rm = TRUE, bw = "nrd",
                                    adjust = 1.2), silent = TRUE)
    if (class(tryd) != "try-error") {
      d$y <- d$y/max(d$y)
      graphics::lines(d)
    }
  }

  "panel_cor" <- function(x, y, prefix = "", ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y, use = "pairwise", method = cor_method)
    # Add background fill
    colors <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
                "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    col_fill <- colors[findInterval(r, seq(-1, 1, length.out = 12), all.inside = TRUE)]
    # Scale box size
    r_range <- seq(0, 1, length.out = 200)
    fancy_size <- 4/3 - 4/3*exp(-log(2)/0.5 * r_range)
    adj <- fancy_size[findInterval(abs(r), r_range)] * 0.5
    graphics::polygon(x = c(0.5-adj, 0.5+adj, 0.5+adj, 0.5-adj),
                      y = c(0.5+adj, 0.5+adj, 0.5-adj, 0.5-adj),
                      border = col_fill, col = col_fill)
    txt <- format(c(round(r, 2), 0.123456789), digits = 2)[1]
    txt <- paste0(prefix, txt)
    if (scale) {
      cex1 <- adj/0.5 + 1
      graphics::text(0.5, 0.5, txt, cex = cex1)
    } else {
      graphics::text(0.5, 0.5, txt, cex = 2)
    }
  }

  "panel_smoother" <- function(x, y, pch = graphics::par("pch"),
                               col.smooth = "red", span = smooth_span, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- stats::sd(x, na.rm = TRUE)
    ys <- stats::sd(y, na.rm = TRUE)
    r = stats::cor(x, y, use = "pairwise", method = cor_method)
    if (jitter) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    graphics::points(x, y, pch = pch)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      graphics::lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                      col = col.smooth)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::pairs(x, diag.panel = panel_hist_density, lower.panel = panel_smoother,
                  upper.panel = panel_cor, pch = pch)

}

