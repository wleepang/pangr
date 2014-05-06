#' Heatmap with colorbar
#' 
#' Draws a heatmap based on matrix data with an optional associated colorbar
#' 
#' @param M Matrix of values to plot
#' @param nlev Number of color levels, default: 32.
#' @param colorbar Boolean, TRUE to show colorbar
#' @param zlab Axis label for colorbar. NULL and NA values suppress label. default: NULL
#' @param zticks Number of z-axis tick marks
#' @param crp Color ramp function (e.g. as returned by \code{colorRampPalette()})
#' @param ... Additional arguments passed to \code{image()}
#' 
#' @details
#' Plots both the matrix heatmap and colorbar (if specified) using \code{image()}.
#' Placement of each subplot is done using \code{par(fig=...)}.  Thus, this
#' function is not compatible with \code{layout()} or \code{par(mfrow=..., mfcol=...)}.
#' 
#' Yet to be determined if this function works with \code{split.screen}.
#' 
#' @return
#' \code{NULL} is returned invisibly.
#' 
#' @seealso
#' \link{image}, \link{colorRampPalette}
hmcb = function(M, nlev=32, colorbar=T, rug=F, zlab=NULL, zticks=5, crp=colorRampPalette(c('white', 'red')), ...) {
  zlev = function(n, zlim=NULL) {
    if (is.null(zlim)) {
      return(seq(min(M), max(M), length.out=n))
    } else {
      return(seq(zlim[1], zlim[2], length.out=n))
    }
  }
  
  par.def = par(no.readonly=T)
  
  # main heatmap
  # note: matrix rows are mapped to x-axis
  par(fig=c(0,.8,0,1), mar=c(5.1, 4.1, 4.1, 0.1))
  image(x=1:nrow(M), y=1:ncol(M), z=M, col=crp(nlev), ...)
  
  # colorbar
  if (colorbar) {
    args = list(...)
    par(fig=c(0.8,1,0,1), mar=c(5.1, 0.1, 4.1, 4.1), new=T)
    image(y=1:nlev, z=matrix(zlev(nlev, zlim=args$zlim), nrow=1), col=crp(nlev), xaxt='n', yaxt='n', bty='n', ann=F)
    
    if (rug) {
      rug.pts = (M-min(zlev(nlev, zlim=args$zlim)))/diff(range(zlev(nlev, zlim=args$zlim)))
      rug.pts = rug.pts * (nlev - 1) + 1
      points(jitter(rep(0, length(M)), amount=0.25), rug.pts, pch=19, col=adjustcolor('black', alpha.f=0.5), cex=0.7)
    }
    
    axis(4, at=seq(1,nlev,length.out=5), labels=prettyNum(zlev(5, zlim=args$zlim)))
    if (!is.null(zlab) && !is.na(zlab)) {
      mtext(zlab, side=4, line=3)
    }
  }
  
  suppressWarnings({par(par.def)})
  invisible(NULL)
}