##' Adaptive smoothing using splines
##'
##' Performs jack-knifed and bagged spline smoothing
##' 
##' @param x An \code{xy.coords} object, \code{list()} with named numeric elements \code{x}
##'   and \code{y}, or an atomic numeric vector of x-coordinates.  If an atomic vector
##'   the parameter \code{y} is required.
##' 
##' @param y Required if the parameter \code{x} is specified as an atomic vector.
##'   An atomic vector of y-coordinates.
##' 
##' @param pct Fraction of randomly selected data points excluded during each
##'   fitting iteration.  Specified on the interval [0,1].
##' 
##' @param niter Number of fits to perform and aggregate into the final curve.
##'   If not specified defaults to 10x more than the number of data points
##'   corresponding to \code{pct}.
##' 
##' @return A list with components
##' \describe{
##'   \item{x}{x-coordinates of the smoothed curve}
##'   \item{y}{y-coordinates of the smoothed curve}
##'   \item{pct}{fraction of data excluded}
##'   \item{niter}{number of fitting iterations performed}
##'   }
##' @export
##' @seealso \link[stats]{smooth.spline} \link[stats]{spline}
smooth.adaptive.spline = function(x, y=NULL, pct=0.1, niter=NULL) {
  
  if (is.recursive(x) && !is.null(x$y)) {
    xy.in = x
  } else {
    xy.in = list(x=x, y=y)
  }
  
  if (is.null(niter)) {
    niter = ceiling(length(xy.in$x)*pct)*10
  }
  
  XY = list(pct=pct, niter=niter)
  for (i in 1:niter) {
    set.seed(i) # this ensures that the same fit happens with repeated runs
    ix = sort(sample(seq_along(xy.in$x), floor((1-pct)*length(xy.in$x))))
    
    xy = list(x=xy.in$x[ix], y=xy.in$y[ix])
    
    xy.s = smooth.spline(xy)           # smoothing of sampled data
    xy.ss = spline(xy.s, xout=xy.in$x) # spline interpolate to full range
    
    if (i == 1) {
      # first iteration, set trajectory
      XY[['x']]=xy.ss$x
      XY[['y']]=xy.ss$y
    } else {
      # gather the mean trajectory of all iterations
      XY$y = (XY$y + xy.ss$y) / 2
    }
  }
  
  return(XY)
  
}

##' Adaptive smoothing using optimized loess
##'
##' Performs loess smoothing with adaptive parameter optimization
##' 
##' @param x An \code{xy.coords} object, \code{list()} with named numeric elements \code{x}
##'   and \code{y}, or an atomic numeric vector of x-coordinates.  If an atomic vector
##'   the parameter \code{y} is required.
##' 
##' @param y Required if the parameter \code{x} is specified as an atomic vector.
##'   An atomic vector of y-coordinates.
##' 
##' @param span.interval The interval of \code{span} values, used by \link[stats]{loess},
##'   over which to \link[stats]{optimize} the fit. The optimization minimizes the value
##'   of (sum of squared residuals)/span.
##' 
##' @return A list with components
##' \describe{
##'   \item{x}{x-coordinates of the smoothed curve}
##'   \item{y}{y-coordinates of the smoothed curve}
##'   \item{span}{the \code{span} value used for the optimized fit}
##'   \item{objective}{the final value of the objective function}
##'   }
##' @export
##' @seealso \link[stats]{loess} \link[stats]{optimize}
smooth.adaptive.loess = function(x, y=NULL, span.interval=c(0.1, 0.7)) {
  # uses loess (local weighted polynomial fitting) to smooth curve
  # optimizes the span parameter to fit the data but avoid over-smoothing
  
  if (is.null(y) & ! is.list(x)) {
    stop('first argument must be xy.coords')
  }
  
  if (is.null(y) & is.list(x)) {
    y = x$y
    x = x$x
  }
  
  span.opt = optimize(function(span, x, y){
    y.loess = loess(y~x, span=span, data.frame(x=x, y=y))
    sse = sum(y.loess$residuals^2)
    opt = sse/span
    
    return(opt)
    
  }, span.interval, x, y, maximum=FALSE)
  
  y.loess = loess(y~x, span=span.opt$minimum, data.frame(x=x, y=y))
  y.predict = predict(y.loess, data.frame(x=x))
  
  return(list(x=x, y=unname(y.predict), span=span.opt$minimum, objective=span.opt$objective))
}

##' Wrapper for \code{loess()} that uses \code{xy.coords}
##' 
##' @param x An \code{xy.coords} object, \code{list()} with named numeric elements \code{x}
##'   and \code{y}, or an atomic numeric vector of x-coordinates.  If an atomic vector
##'   the parameter \code{y} is required.
##' 
##' @param y Required if the parameter \code{x} is specified as an atomic vector.
##'   An atomic vector of y-coordinates.
##' 
##' @param ... Additional arguments to be passed to \code{loess()}
##' 
##' @return A list with components
##'   \describe{
##'     \item{x}{x-coordinates of the smoothed curve}
##'     \item{y}{y-coordinates of the smoothed curve}
##'     \item{loess}{object returned by \code{loess()}}
##'   }
##' 
##' @seealso \link[stats]{loess}
##' @export
loess.xy.coords = function(x, y=NULL, ...) {
  if (is.null(y) & ! is.list(x)) {
    stop('First argument `x` must be xy.coords if `y` is NULL')
  }
  
  if (is.null(y) & is.list(x)) {
    y = x$y
    x = x$x
  }
  
  obj = loess(y~x, list(x=x, y=y), ...)
  y.s = predict(obj)
  
  return(list(x=x, y=y.s, loess=obj))
}

##' Adaptive smoothing using optimized lowess
##'
##' Performs lowess smoothing with adaptive parameter optimization
##' 
##' @param x An \code{xy.coords} object, \code{list()} with named numeric elements \code{x}
##'   and \code{y}, or an atomic numeric vector of x-coordinates.  If an atomic vector
##'   the parameter \code{y} is required.
##' 
##' @param y Required if the parameter \code{x} is specified as an atomic vector.
##'   An atomic vector of y-coordinates.
##' 
##' @param span.interval The interval of \code{f} values, used by \link[stats]{lowess},
##'   over which to \link[stats]{optimize} the fit. The optimization minimizes the value
##'   of (sum of squared residuals)/span.
##' 
##' @return A list with components
##' \describe{
##'   \item{x}{x-coordinates of the smoothed curve}
##'   \item{y}{y-coordinates of the smoothed curve}
##'   \item{span}{the \code{span} value used for the optimized fit}
##'   \item{objective}{the final value of the objective function}
##'   }
##' @export
##' @seealso \link[stats]{lowess} \link[stats]{optimize}
smooth.adaptive.lowess = function(x, y=NULL, span.interval=c(0.1, 0.7)) {
  # uses loess (local weighted polynomial fitting) to smooth curve
  # optimizes the span parameter to fit the data but avoid over-smoothing
  
  if (is.null(y) & ! is.list(x)) {
    stop('first argument must be xy.coords')
  }
  
  if (is.null(y) & is.list(x)) {
    y = x$y
    x = x$x
  }
  
  span.opt = optimize(function(span, x, y){
    y.lowess = lowess(x, y, f=span)$y
    sse = sum((y-y.lowess)^2)
    opt = sse/span
    
    return(opt)
    
  }, span.interval, x, y, maximum=FALSE)
  
  y.lowess = lowess(x, y, f=span.opt$minimum)$y
  
  return(list(x=x, y=y.lowess, span=span.opt$minimum, objective=span.opt$objective))
}