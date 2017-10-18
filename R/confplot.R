#' Plot Confidence Bands
#'
#' Plot confidence bands of lower and upper y values as a filled area, or add
#' polygon to an existing plot.
#'
#' @param x a numeric vector of x values. Alternatively, \code{x} can be a
#'        matrix or data frame containing x values in the first column and lower
#'        and upper y values in the next two columns.
#' @param y1 a numeric vector of lower y values. Alternatively, \code{y1} can be
#'        a matrix or data frame containing lower and upper y values in two
#'        columns.
#' @param y2 a numeric vector of upper y values, if not already supplied in
#'        \code{x} or \code{y1}.
#' @param add whether confidence bands should be added to an existing plot.
#' @param xlab a label for x axis.
#' @param ylab a label for y axis.
#' @param border border color of polygon. The default \code{NA} is to omit
#'        borders.
#' @param col fill color of polygon.
#' @param formula a \code{\link{formula}}, such as \code{cbind(y1,y2)~x},
#'        specifying x and y values.
#' @param data a data frame (or list) from which the variables in formula should
#'        be taken.
#' @param subset an optional vector specifying a subset of observations to be
#'        used.
#' @param na.action a function which indicates what should happen when the data
#'        contain \code{NA} values. The default is to ignore missing values in
#'        the given variables.
#' @param \dots further arguments passed to \code{confplot.default},
#'        \code{matplot}, and \code{polygon}.
#'
#' @return
#' Data frame of coordinates that were used for plotting.
#'
#' @seealso
#' \code{\link{polygon}} is the underlying function used to draw polygons.
#'
#' \code{\link{areaplot}} produces a stacked area plot.
#'
#' \code{\link{areaplot-package}} gives an overview of the package.
#'
#' The \pkg{gplots} and \pkg{plotrix} packages provide functions to plot error bars.
#'
#' @importFrom graphics matplot polygon
#'
#' @examples
#' model <- lm(log(dist)~log(speed), cars)
#' ci95 <- predict(model, data.frame(speed=4:25), interval="confidence")
#' ci50 <- predict(model, data.frame(speed=4:25), interval="confidence", level=0.5)
#' x <- log(4:25)
#' y1 <- ci95[,"lwr"]
#' y2 <- ci95[,"upr"]
#' mydata <- data.frame(x, y1, y2)
#'
#' # Input format
#' confplot(x, y1, y2)               # vectors
#' confplot(x, cbind(y1,y2))         # y values in 2 columns
#' confplot(mydata)                  # data in 3 columns
#' confplot(cbind(y1,y2)~x, mydata)  # formula
#'
#' # Overlay
#' plot(log(dist)~log(speed), cars, type="n")
#' confplot(x, ci95[,2:3], add=TRUE)
#' confplot(x, ci50[,2:3], add=TRUE, col="darkgray")
#' lines(x, ci95[,1])
#' points(log(dist)~log(speed), cars)
#'
#' @export

confplot <- function(x, ...)
{
  UseMethod("confplot")
}

#' @rdname confplot
#' @export
#' @export confplot.default

confplot.default <- function(x, y1=NULL, y2=NULL, add=FALSE, xlab=NULL,
                             ylab=NULL, border=NA, col="lightgray", ...)
{
  if(is.vector(x))
  {
    if(is.null(xlab))
      xlab <- deparse(substitute(x))
    if(is.null(y1))
      stop("'y1' should not be NULL when 'x' is a vector")
    if(is.vector(y1) && is.null(y2))
      stop("'y2' should not be NULL when 'x' and 'y1' are vectors")
    if(!is.null(ncol(y1)))
    {
      if(ncol(y1) != 2)
        stop("'y1' should be a vector or contain 2 columns")
      y2 <- y1[,2]
      y1 <- y1[,1]
    }
  }
  else if(ncol(x) == 3)
  {
    if(is.null(xlab))
      xlab <- colnames(x)[1]
    y1 <- x[,2]
    y2 <- x[,3]
    x <- x[,1]
  }
  else
    stop("'x' should be a vector or contain 3 columns")

  if(is.null(ylab))
    ylab <- ""

  na <- is.na(x) | is.na(y1) | is.na(y2)
  x <- x[!na][order(x[!na])]
  y1 <- y1[!na][order(x[!na])]
  y2 <- y2[!na][order(x[!na])]

  if(!add)
    suppressWarnings(matplot(range(x), range(c(y1,y2)), type="n",
                             xlab=xlab, ylab=ylab, ...))
  polygon(c(x,rev(x)), c(y1,rev(y2)), border=border, col=col, ...)

  invisible(data.frame(x, y1, y2))
}

#' @rdname confplot
#' @export
#' @export confplot.formula

confplot.formula <- function (formula, data, subset, na.action=NULL, ...)
{
  m <- match.call(expand.dots=FALSE)
  if(is.matrix(eval(m$data,parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")

  mf <- eval(m, parent.frame())
  if(is.matrix(mf[[1]]))
  {
    lhs <- as.data.frame(mf[[1]])
    names(lhs) <- as.character(m[[2]][[2]])[-1]
    confplot.default(cbind(mf[-1],lhs), ...)
  }
  else
  {
    confplot.default(mf[2:1], ...)
  }
}
