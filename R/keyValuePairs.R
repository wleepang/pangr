#' Split a key-value pair list string
#' 
#' @param s Character vector of key-value pair list strings
#' @param pair.split Regex pattern that splits individual key-value pairs. Default ";"
#' @param kv.split Regex pattern that splits keys and values. Default ":"
#' 
#' @details
#' Essentially a recursive wrapper for \code{strsplit} that is \code{lapply}'d over \code{s}
#' 
#' @seealso
#' \link{strsplit}, \link{lapply}.
#' In addtion, \link[pangr]{getKeyValue} is a wrapper that reduces parsing and 
#' extracting a single key-value to a single function call.
#' 
#' @return
#' A list the same length as \code{s} whose elements are lists whose names are 
#' extracted keys and elements are extracted values.
kvp = function(s, pair.split=';', kv.split=':') {
  # splits a string of delimited key-value pairs into a heirarchical named list
  # with names as keys
  
  # top level list of key-val pairs
  L = strsplit(s, split=pair.split)
  
  KV = lapply(L, function(l) {
    kv = strsplit(l, split=kv.split)
    k = sapply(kv, '[', 1)
    v = sapply(kv, '[', 2)
    
    kv = as.list(v)
    names(kv) = k
    
    return(kv)
  })
  
  return(KV)
}

#' Get a key value from a key-value pair list string
#' 
#' @param s Character vector of key-value pair list strings
#' @param key Character vector of length 1 of the key to be extracted
#' @param val.fun Function to apply to the extracted values
#' 
#' @details
#' A wrapper for \link[pangr]{kvp}
#' 
#' @seealso
#' \link[pangr]{kvp}
#' 
#' @return
#' A vector the same length as \code{s} of values defined by \code{key} and
#' and transformed by \code{val.fun}
getKeyValue = function(s, key, val.fun=identity, ...) {
  # extracts a key associated value from a string of delimited key-value pairs
  # converts the output according to val.fun(x, ...)
  val.fun(sapply(kvp(as.character(s)), '[[', key), ...)
}
