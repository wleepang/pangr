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
#' @section DEPRECATED
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
#' 
#' @return
#' A list the same length as \code{x} where each element has named elements
#' corresponding to the named groups in \code{pattern}.
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
