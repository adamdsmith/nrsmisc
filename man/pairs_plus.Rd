% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairs_plus.R
\name{pairs_plus}
\alias{pairs_plus}
\title{Create matrix of scatterplots displaying pairwise correlation information,
 LOWESS smooth, and histogram/densityinformation}
\usage{
pairs_plus(x, cor_method = c("spearman", "pearson", "kendall"),
  scale = TRUE, pch = 20, jitter = FALSE, factor = 2,
  smooth_span = 2/3)
}
\arguments{
\item{x}{matrix or data.frame with columns (variables) for which to generate a matrix
of scatterplots (like \code{\link[graphics]{pairs}}) but with additional density
and correlation data to assist data exploration. NOTE that logical, character, and factor
columns are converted to integers, which may or may not be useful.}

\item{cor_method}{character scalar indicating which correlation coefficient to compute.
See \code{\link[stats]{cor}}. One of "spearman" (defaul), "pearson", or "kendall".}

\item{scale}{logicial (default = TRUE); scale text indicating correlation coefficient
with absolute magnitude of correlation?}

\item{pch}{An integer specifying a symbol or a single character to be used when
plotting points. See \code{\link[graphics]{points}} for possible values and their
interpretation.}

\item{jitter}{logical (default = FALSE); should values have a small amount of noise
added to aid differentiation? If TRUE, control jitter amount with \code{factor}
argument. See \code{\link[base]{jitter}}}

\item{factor}{numeric to change amount of jitter. See details of \code{\link[base]{jitter}}
for explanation.}

\item{smooth_span}{smoother span; the proportion of points in the plot which influence
the smoothness of the fitted LOWESS line. Restricted to (0, 1) here. Default is 2/3.}
}
\description{
\code{\link[graphics]{pairs}} on steroids. Intended for use as one component of
 exploratory data analysis
}
\examples{
\dontrun{
a <- sort(runif(100))
df <- data.frame(super = a,
                 cali = a + rnorm(100, 0, 0.5),
                 fragi = rev(a) + rnorm(100, 0, 1),
                 listic = (a + rnorm(100, 0, 0.25)) ^ 2,
                 expi = rep(LETTERS[1:4], each = 25),
                 ali = a - rnorm(100, 0, 1.5),
                 docious = a + rnorm(100, 0, 2))
pairs_plus(df)
pairs_plus(df, scale = FALSE)
pairs_plus(df, smooth_span = 1/4)
pairs_plus(df, cor_method = "pearson")
}
}
