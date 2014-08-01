# a cache of helpful utility functions that make using R more sane

#' Convert colors to hexidecimal codes
#' 
#' @param col A vector or matrix of either numeric colors or color names
#' @param alpha The alpha level of the output color ranging from [0,255].
#'   Default is 255 (opaque).
#' 
#' @details
#' Uses the color's underlying RGB space to generate the hex code.
#' 
#' @section DEPRECATED:
#' Use \link[grDevices]{adjustcolor} instead.
#' 
#' @return
#' An object of the same type and dimensions as \code{col} with all entries replaced
#' with hexidecimal string equivalents
#' 
#' @seealso
#' \link[grDevices]{adjustcolor} performs the same function and has a few more options
col2hex = function(col, alpha=255) {
  # converts named or integer specified colors to a hex string with alpha values
  # input col can be scalar, vector, or matrix
  # default alpha level is 255 (opaque)
  
  rgba = sprintf('#%s%02x', apply(col2rgb(col), 2, function(x){paste(sprintf('%02x',x), sep='', collapse='')}), as.integer(alpha))
  if (class(col) == 'matrix') {
    rgba = matrix(rgba, nrow=nrow(col))
  }
  return(rgba)
}

#' Create decorative lines for console output
#' 
#' @param char Character to use to create the line. Default is \code{'-'}
#' @param width Width in characters of the line. Default is 80.
#' 
#' @details
#' A simple wrapper for \code{past(rep(...), collapse='')}.
#' 
#' @return
#' A string with \code{char} replicated \code{width} times that one could use
#' for console message display or other text based output.
#' 
#' @export
hline = function(char='-', width=80) paste(rep(char, times=width), collapse='')
# produces a text line of 'char' as wide as 'width'
# useful for prettifying console outputs

dropNACols = function(DF, FUN=function(.x) {!all(.x)}) {
  DF[,sapply(DF, function(.x) {FUN(is.na(.x))} )]
}
# drops columns from a data.frame that are all NA

nan2na = function(x) {
  # converts NaN valus in x to NA
  x[which(is.nan(x))] = NA
  return(x)
}

makeFinite = function(x, bignum) {
  x[which(is.infinite(x))] = sign(x[which(is.infinite(x))])*bignum
  return(x)
}

re.capture = function(pattern, string, ...) {
  rex = list(src=string, 
             result=regexpr(pattern, string, perl=TRUE, ...), 
             names=list())
  
  for (.name in attr(rex$result, 'capture.name')) {
    rex$names[[.name]] = substr(rex$src, 
                                attr(rex$result, 'capture.start')[,.name], 
                                attr(rex$result, 'capture.start')[,.name]
                                + attr(rex$result, 'capture.length')[,.name]
                                - 1)
  }
  
  return(rex)
}

#' Global RegEx processing with named capture
#' 
#' @param pattern Regex pattern with named capturing groups
#' @param x Character vector where \code{pattern} is searched for in each element
#' @param ... Additional arguments passed to \link[base]{gregexpr}.
#' 
#' @details
#' A wrapper for \link[base]{gregexpr} that facilitates retrieval of named
#' capture groups
#' 
#' @return
#' A list the same length as \code{x} where each element has named elements
#' corresponding to the named groups in \code{pattern}.
#' 
#' @seealso
#' \link[base]{gregexpr}
#' 
#' @examples
#' > x = '`a` + `[b]` + `[1c]` + `[d] e`'
#' > z = '`f` + `[g]` + `[1h]` + `[i] j`'
#' > gregexcap('`(?<tok>.*?)`', x)
#' [[1]]
#' [[1]]$tok
#' [1] "a"     "[b]"   "[1c]"  "[d] e"
#' 
#' > gregexcap('`(?<tok>.*?)`', c(x,z))
#' [[1]]
#' [[1]]$tok
#' [1] "a"     "[b]"   "[1c]"  "[d] e"
#'  
#' [[2]]
#' [[2]]$tok
#' [1] "f"      "[g]"    "[1h]"  "[i] j"
#' 
#' @export
gregexcap = function(pattern, x, ...) {
  args = list(...)
  args[['perl']] = T
  
  re = do.call(gregexpr, c(list(pattern, x), args))
  
  mapply(function(re, x){
    
    cap = sapply(attr(re, 'capture.names'), function(n, re, x){
      start = attr(re, 'capture.start')[, n]
      len   = attr(re, 'capture.length')[, n]
      end   = start + len - 1
      tok   = substr(rep(x, length(start)), start, end)
      
      return(tok)
    }, re, x, simplify=F, USE.NAMES=T)
    
    return(cap)
  }, re, x, SIMPLIFY=F)
  
}

uniquify = function(SET, target.col=1, uniquifier.col=2) {
  unq = unique(SET[,target.col])
  # count how many times a column name is used
  # select names used more than once
  not.unq = unq[which(sapply(unq, function(n) {sum(as.numeric(SET[,target.col] %in% n))}) > 1)]
  
  # uniquify repeated names using measurement annotation
  keys = apply(SET, 1, function(r) {ifelse(r[target.col] %in% not.unq, paste(r[c(target.col,uniquifier.col)], collapse='.'), r[1])})
  return(keys)
}

#' Check if a value is within an interval
#' 
#' @param x A numeric vector of values to test
#' @param interval A numeric vector with at least 2 unique values that defines
#'   the interval over which the value of \code{x} must reside
#' @param endpts A character value that determines how endpoints are treated to
#'   determine "between-ness":
#'   \describe{
#'    \item{both}{(Default) \code{TRUE} if min(interval) <= x <= max(interval)}
#'    \item{lb}{\code{TRUE} if min(interval) <= x < max(interval)}
#'    \item{ub}{\code{TRUE} if min(interval) < x <= max(interval)}
#'    \item{none}{\code{TRUE} if min(interval) < x < max(interval)}
#'   }
#' 
#' @return
#' A logical vector the same size as \code{x} that is \code{TRUE} for elements
#' of \code{x} that satisfy the conditions defined by \code{interval} and \code{endpts},
#' and \code{FALSE} otherwise.
#' 
#' @export
is.between = function(x, interval, na.rm=T, endpts=c('both', 'lb', 'ub', 'none')) {
  lb = min(interval, na.rm=na.rm)
  ub = max(interval, na.rm=na.rm)
  TF = switch(match.arg(endpts), 
              lb = {
                x < ub & x >= lb
              }, ub = {
                x <= ub & x > lb
              }, none = {
                x < ub & x > lb
              }, {
                # both, is default case
                x <= ub & x >= lb
              })
  
  return(TF)
}

MultiSetFun = function(fun, x) {
  # performs a set function (e.g. intersect, setdiff) on x
  # where x is a list of vectors of the same type but not necessarily the same length
  
  fun = tolower(fun)
  
  this.intersect = function(M, v) {
    return(names(which(rowSums(M) == sum(v))))
  }
  
  this.setdiff = function(M, v) {
    D = lapply(v, function(i){
      names(which(rowSums(M) == i))
    })
    names(D) = colnames(M)
    return(D)
  }
  
  this.union = function(M, v) {
    return(rownames(M))
  }
  
  # concatenate all unique elements in x - this is effectively a union
  U = sort(unique(do.call('c', unname(x))))
  
  # set truth map, (i,j) is true if element i is in set j
  M = as.matrix(as.data.frame(lapply(x, function(l){U %in% l})))
  rownames(M) = U
  
  # apply column values
  v = 2^seq(0, ncol(M)-1)
  M = t(t(M) * v)
  
  return(
    switch(fun, 
           intersect = this.intersect(M,v),
           setdiff   = this.setdiff(M,v),
           union     = this.union(M,v),
           NULL) )
  
}

#' Determine local extrema
#' 
#' Determines the coordinates of local extrema in a series and identifies them as
#' either maxima or minima.
#' 
#' @param x Either an \link{xy.coords} object or an atomic numeric vector of x-coordinates
#' @param y Required if \code{x} is atomic, an atomic numeric vector of y-coordinates
#'   the same length as \code{x}.
#' @param ddytol Tolerance for values of the 2nd derivative to flag candidate points as extrema.
#'   Default (NULL) automatically computes as 10% of the data range.
#' @param fx Multiplicative factor by which the number of points in the data is
#'   expanded (via \link{splinefun}) to refine extrema location detection. Default: 1000.
#'
#' @details
#' The data series is first smoothed using adaptive loess and then spline interpolated
#' between points.  Zero-crossings of the first derivative of spline interpolated data 
#' are extracted as local extrema.  The nearest 10 points (5-pre, 5-post) surrounding
#' the extrema are used to determine if the extrema is a local minimum or local maximum.
#' 
#' @return
#' An \link{xy.coords} compatible list with the following elements:
#' \describe{
#'  \item{x}{x-coordinates of local extrema}
#'  \item{y}{y-coordinates of local extrema}
#'  \item{indices}{indices of local extrema in the original data}
#'  \item{is.maxima}{logical vector the same length as indices where \code{TRUE} indicates a local maximum}
#' }
#' 
#' @seealso
#' \link{xy.coords}, \link{smooth.adaptive.loess}, \link{splinefun}
#' 
#' @export
extrema = function(x, y=NULL, ddytol=NULL, fx=1000) {
  # locates local extrema
  
  # x can be either an atomic vector or an xy.coords list
  if (is.atomic(x)) {
    if(is.null(y)) {
      xy = list(x=seq_along(x), y=x)  
    } else {
      if (length(x) != length(y)) {
        stop('x and y not of same length')
      }
      xy = list(x=x, y=y)  
    }
    
  } else if (is.recursive(x)) {
    if (is.null(x$x) || is.null(x$y)) {
      stop('x not an xy.coords object')
    }
    xy = x
  }
  
  # smooth xy using adaptive loess
  xys = smooth.adaptive.loess(xy)
  dxys = smooth.adaptive.loess(list(x=xys$x, y=splinefun(xys)(xys$x, deriv=1)))
  
  # expand the number of points in xy by factor `fx` and spline interpolate
  sx = seq(xy$x[1], range(xy$x)[2], length.out=length(xy$x)*fx)
  sy = splinefun(xys)(sx)
  dy = splinefun(xys)(sx, deriv=1)
  dxy = list(x=sx, y=dy)
  
  ddy= splinefun(dxys)(sx, deriv=1)
  ddxy = list(x=sx, y=ddy)
  
  # locate zero crossings
  if (is.null(ddytol)) {
    ddytol = diff(range(ddy))*0.1
  }
  zcv = abs(diff(sign(dy))) # note: will be 1-point less than input data
  zcx = sx[sort(intersect(which(zcv > 0), which(abs(ddy) > ddytol)))]
  zcy = sy[sort(intersect(which(zcv > 0), which(abs(ddy) > ddytol)))]
  
  zc = list(x = zcx, y = zcy, v = c(zcv, rev(zcv)[1]))
  
  # reduce to "real" points/indices in the original data set
  iz = unique(sapply(zcx, function(z){which(xy$x > z)[1]}))-1
  
  # distinguish between maxima and minima
  bpts = do.call(cbind, lapply(c(-5:-1, 1:5), function(n){
    if (iz+n < 1 || iz+n > length(xys$y)) {
      return(NA)
    }
    xys$y[iz+n]
  }))
  is.maxima = xys$y[iz] > rowMeans(bpts, na.rm = T)
  
  return(c(lapply(xy, '[', iz), list(indices=iz, is.maxima=is.maxima, deriv=list(dxy, ddxy), zc=zc)))
}

#' Pad a data.matrix reflectively
#' 
#' @param X Data matrix to pad
#' @param wsz number of data points to pad with
#' 
#' @return
#' A padded data matrix with \code{2*wsz} additional rows.
padMatrix <- function (X, wsz) {
  # pads a matrix by reflecting the data within a specified window of the ends
  
  n = nrow(X)
  
  # since R is column based, transpose the input matrix
  # so that padding appends data column-wise: [..., p2, p1, data-cols, p1, p2, ...]
  X = t(X)
  LH = 2*X[,1] - X[,wsz:2]
  RH = 2*X[,n] - X[,(n-1):(n-wsz)]
  Y = t(cbind(LH, X, RH))
  
  return(Y)
}