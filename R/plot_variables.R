#' @param method character scalar indicating which correlation coefficient to compute.
#'  See \code{\link[stats]{cor}}. One of "spearman" (defaul), "pearson", or "kendall".
#' @param jitter logical (default = FALSE) should values have a small amount of noise
#'  added to aid differentiation? If TRUE, control jitter amount with \code{factor}
#'  argument. See \code{\link[base]{jitter}}
#' @param pch An integer specifying a symbol or a single character to be used when
#'  plotting points. See \code{\link[graphics]{points}} for possible values and their
#'  interpretation.

pairs.panels <- function (x, science = c("biol", "phys"), scale = TRUE,
                          density = TRUE,
                          pch = 20, cor = TRUE,  method = c("spearman", "pearson", "kendall"),
                          jitter = FALSE, factor = 2, hist.col = "lightblue3", ...)
{
  science <- match.arg(science)
  method <- match.arg(method)
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
    rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    if (density) {
      tryd <- try(d <- stats::density(x, na.rm = TRUE, bw = "nrd",
                                      adjust = 1.2), silent = TRUE)
      if (class(tryd) != "try-error") {
        d$y <- d$y/max(d$y)
        graphics::lines(d)
      }
    }
  }

  "panel_cor" <- function(x, y, prefix = "", ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y, use = "pairwise", method = method)
    # Add background fill
    colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
                "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    col_fill <- colors[findInterval(r, seq(-1, 1, length.out = 11))]
    # Scale box size
    r.range <- seq(0, 1, length.out = 200)
    if (science == "biol")
      fancy.size <- 4/3 - 4/3*exp(-log(2)/0.5 * r.range)
    else
      fancy.size <- scale_vec(exp(4*r.range) - 1, c(0, 1))
    adj <- fancy.size[findInterval(abs(r), r.range)] * 0.5
    graphics::polygon(x = c(0.5-adj, 0.5+adj, 0.5+adj, 0.5-adj),
                      y = c(0.5+adj, 0.5+adj, 0.5-adj, 0.5-adj),
                      border = col_fill, col = col_fill)
    txt <- format(c(round(r, 2), 0.123456789), digits = 2)[1]
    txt <- paste(prefix, txt, sep = "")
    cex <- 8/graphics::strwidth(txt)
    if (scale) {
      cex1 <- cex * adj * 2
      if (cex1 < 0.25)
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    } else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }

  "panel_smoother" <- function(x, y, pch = graphics::par("pch"),
                                            col.smooth = "red", span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jitter) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    graphics::points(x, y, pch = pch, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }

  pairs(x, diag.panel = panel_hist_density, upper.panel = panel_smoother,
          lower.panel = panel_cor, pch = pch, ...)

}
