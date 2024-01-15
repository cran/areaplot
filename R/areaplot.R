#' Area Plot
#'
#' Produce a stacked area plot, or add polygons to an existing plot.
#'
#' @param x a numeric vector of x values, or if \code{y=NULL} a numeric vector
#'        of y values. Can also be a 1-dimensional table (x values in names, y
#'        values in array), matrix or 2-dimensional table (x values in row names
#'        and y values in columns), a data frame (x values in first column and y
#'        values in subsequent columns), or a time-series object of class
#'        \code{ts/mts}.
#' @param y a numeric vector of y values, or a matrix containing y values in
#'        columns.
#' @param rev whether to plot the stacked areas from bottom to top, instead of
#'        top to bottom.
#' @param prop whether data should be plotted as proportions, so stacked areas
#'        equal 1.
#' @param add whether polygons should be added to an existing plot.
#' @param xlab a label for x axis.
#' @param ylab a label for y axis.
#' @param col fill color of polygon(s). The default is a vector of gray colors.
#' @param legend a logical indicating whether a legend should be added, or a
#'        vector of strings for the legend. This only applies when more than one
#'        series is plotted.
#' @param args.legend a list of additional arguments to pass to the
#'        \code{\link{legend}} function.
#' @param formula a \code{\link{formula}}, such as \code{y~x},
#'        \code{cbind(y1,y2)~x}, or \code{y~x+group}, specifying x and y values.
#'        A dot on the left-hand side, \code{.~x}, means all variables except
#'        the one specified on the right-hand side.
#' @param data a data frame (or list) from which the variables in formula should
#'        be taken.
#' @param subset an optional vector specifying a subset of observations to be
#'        used.
#' @param na.action a function which indicates what should happen when the data
#'        contain \code{NA} values. The default is to ignore missing values in
#'        the given variables.
#' @param \dots further arguments passed to \code{areaplot.default},
#'        \code{matplot}, and \code{polygon}.
#'
#' @return Matrix of cumulative sums that was used for plotting.
#'
#' @seealso
#' \code{\link{polygon}} is the underlying function used to draw polygons.
#'
#' \code{\link{confplot}} plots confidence bands as a filled area.
#'
#' \code{\link{areaplot-package}} gives an overview of the package.
#'
#' @examples
#' areaplot(rpois(10,40))
#' areaplot(rnorm(10))
#'
#' # formula
#' areaplot(Armed.Forces~Year, data=longley)
#' areaplot(cbind(Armed.Forces,Unemployed)~Year, data=longley)
#' areaplot(.~Year, data=longley)
#' areaplot(circumference~age+Tree, Orange)
#'
#' # add=TRUE
#' plot(1940:1970, 500*runif(31), ylim=c(0,500))
#' areaplot(Armed.Forces~Year, data=longley, add=TRUE)
#'
#' # data frame
#' mydata <- longley[c("Year","GNP")]
#' areaplot(mydata)
#'
#' # matrix
#' areaplot(WorldPhones)
#' areaplot(WorldPhones, prop=TRUE)
#'
#' # table
#' require(MASS)
#' areaplot(table(Aids2$age))
#' areaplot(table(Aids2$age, Aids2$sex))
#'
#' # ts/mts
#' areaplot(austres)
#' areaplot(Seatbelts[,c("drivers","front","rear")],
#'          ylab="Killed or seriously injured")
#' abline(v=1983+1/12, lty=3)
#'
#' # legend
#' require(MASS)
#' areaplot(table(Aids2$age, Aids2$sex), legend=TRUE, col=c(2,4))
#' areaplot(table(Aids2$age, Aids2$sex), legend=TRUE, col=c(2,4), rev=TRUE)
#' wp <- WorldPhones[,order(colnames(WorldPhones))]
#' areaplot(wp, col=2:8, legend=TRUE, args.legend=list(x="topleft"))
#' areaplot(wp, col=2:8, legend=TRUE, args.legend=list(x="topleft"), rev=TRUE)
#'
#' @importFrom graphics legend matplot polygon
#' @importFrom grDevices gray.colors
#' @importFrom stats is.ts terms time
#'
#' @export

areaplot <- function(x, ...)
{
  UseMethod("areaplot")
}

#' @rdname areaplot
#' @export
#' @export areaplot.default

areaplot.default <- function(x, y=NULL, prop=FALSE, rev=FALSE, add=FALSE,
                             xlab=NULL, ylab=NULL, col=NULL, legend=FALSE,
                             args.legend=NULL, ...)
{
  if(is.ts(x)) # ts/mts
  {
    if(is.null(ylab))
      ylab <- deparse(substitute(x))
    x <- data.frame(Time=time(x), x, check.names=FALSE)
  }
  if(is.table(x))  # table
  {
    if(is.null(ylab))
      ylab <- deparse(substitute(x))
    if(length(dim(x)) == 1)
      x <- t(t(unclass(x)))
    else
      x <- unclass(x)
  }
  if(is.matrix(x))  # matrix
  {
    if(!is.null(rownames(x)) &&
       !any(is.na(suppressWarnings(as.numeric(rownames(x))))))
    {
      x <- data.frame(as.numeric(rownames(x)), x, check.names=FALSE)
      names(x)[1] <- ""
    }
    else
    {
      x <- data.frame(Index=seq_len(nrow(x)), x, check.names=FALSE)
    }
  }
  if(is.list(x))  # data.frame or list
  {
    if(is.null(xlab))
      xlab <- names(x)[1]
    if(is.null(ylab))
    {
      if(length(x) == 2)
        ylab <- names(x)[2]
      else
        ylab <- ""
    }
    y <- x[-1]
    x <- x[[1]]
  }
  if(is.null(y))  # one numeric vector passed, plot it on 1:n
  {
    if(is.null(xlab))
      xlab <- "Index"
    if(is.null(ylab))
      ylab <- deparse(substitute(x))
    y <- x
    x <- seq_along(x)
  }

  if(is.null(xlab))
    xlab <- deparse(substitute(x))
  if(is.null(ylab))
    ylab <- deparse(substitute(y))

  y <- as.matrix(y)
  if(length(x) != nrow(y))
    stop("'x' and 'y' lengths differ")
  if(is.null(col))
    col <- gray.colors(ncol(y))
  col <- rep(col, length.out=ncol(y))
  if(!rev)
  {
    y <- as.matrix(rev(as.data.frame(y)))
    col <- rev(col)
  }
  if(prop)
    y <- prop.table(y, 1)
  y <- t(rbind(0, apply(y, 1, cumsum)))
  na <- is.na(x) | apply(is.na(y), 1, any)
  x <- x[!na][order(x[!na])]
  y <- y[!na,][order(x[!na]),]

  if(!add)
    suppressWarnings(matplot(x, y, type="n", xlab=xlab, ylab=ylab, ...))
  xx <- c(x, rev(x))
  for(i in 1:(ncol(y)-1))
  {
    yy <- c(y[,i+1], rev(y[,i]))
    suppressWarnings(polygon(xx, yy, col=col[i], ...))
  }

  if(is.logical(legend))
    legend <- if(legend && ncol(y)>=3) colnames(y)[-1]
  if(!is.null(legend))
  {
    if(is.null(args.legend))
    {
      legend("topright", legend=rev(legend), fill=rev(col), bty="n", inset=0.02)
    }
    else
    {
      args.legend1 <- list(x="topright", legend=rev(legend), fill=rev(col),
                           bty="n", inset=0.02)
      args.legend1[names(args.legend)] <- args.legend
      do.call("legend", args.legend1)
    }
  }

  invisible(y[,-1])
}

#' @rdname areaplot
#' @importFrom stats xtabs
#' @export
#' @export areaplot.formula

areaplot.formula <- function(formula, data, subset, na.action, xlab=NULL,
                             ylab=NULL, ...)
{
  if(missing(formula) || length(formula) != 3)
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots=FALSE)
  if(is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- m$xlab <- m$ylab <- NULL
  m[[1]] <- quote(model.frame)
  if(formula[[2]] == ".")
  {
    # LHS is .
    rhs <- as.list(attr(terms(formula[-2]), "variables")[-1])
    lhs <- as.call(c(quote(cbind), setdiff(lapply(names(data), as.name), rhs)))
    formula[[2]] <- lhs
    m[[2]] <- formula
  }

  mf <- eval(m, parent.frame())
  if(ncol(mf[-1]) == 0 || ncol(mf[-1]) >= 3)
    stop("formula must specify 1 or 2 variables after the tilde")
  if(anyDuplicated(mf[-1]))
    stop("duplicated values after the tilde - try another formula or subset")
  if(is.null(xlab))
    xlab <- names(mf)[2]
  if(is.matrix(mf[[1]]))
  {
    # LHS is cbind()
    if(ncol(mf[-1]) != 1)
      stop("formula with cbind() must specify 1 variable after the tilde")
    lhs <- mf[[1]]
    rownames(lhs) <- mf[[ncol(mf)]]
    areaplot.default(lhs, xlab=xlab, ylab=ylab, ...)
  }
  else
  {
    if(is.null(ylab))
      ylab <- names(mf)[1]
    areaplot.default(xtabs(mf, addNA=TRUE), xlab=xlab, ylab=ylab, ...)
  }
}
