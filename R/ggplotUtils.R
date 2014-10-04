#' Layout multiple ggplots
#' 
#' @param ... ggplot objects provided as separate arguments
#' @param plotlist A list of ggplot objects
#' @param cols Number of columns the layout should have
#' @param layout A layout matrix. If present \code{cols} is ignored
#' 
#' @details
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' 
#' @section Credits:
#' This function was copied verbatim from the cookbook for R:
#' \link{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#' 
#' Documentation was modified to conform to \link{roxygen2} tagging standards
#' 
#' @export
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Horizontally align ggplots
#' 
#' @param ... \code{ggplot} objects as separate arguments or a single \code{list()}
#'    of ggplot objects to align horizontally
#' @param heights Height(s) for each plot specified with \link[grid]{unit} as defined 
#'    with \link[grid]{grid.layout}.  Default is \code{NULL}, heights are evenly
#'    distributed among plots.
#' @param plot Logical specifying if the result should be plotted.  Default is 
#'    \code{FALSE}
#' 
#' @details
#' Uses low level \link{grid} and \link{gridExtra} operations to compare, modify,
#' and arrange grob widths.
#' 
#' @section Credits:
#' Based on the following StackOverflow Questions:
#' \link{http://stackoverflow.com/questions/13656642/r-align-plot-areas-in-ggplot}
#' \link{http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot}
#' 
#' @return
#' If \code{plot = FALSE}, an \link[gridExtra]{arrangeGrob} object is returned.
#' 
#' @seealso
#' \link[grid]{grid.layout}, \link[gridExtra]{arrangeGrob}, \link[grid]{unit}
#' 
#' @import grid gridExtra
#' @export
gg_halign = function(..., heights = NULL, plot = FALSE) {
  plots = list(...)
  
  if (length(plots) == 1 && inherits(plots[[1]], 'list')) {
    # plots supplied as a single list object
    plots = plots[[1]]
  }
  
  grobs = list()
  widths = list()
  
  for (i in 1:length(plots)) {
    grobs[[i]] = ggplotGrob(plots[[i]])
    widths[[i]] = grobs[[i]]$widths[2:5]
  }
  
  maxWidth = do.call(grid::unit.pmax, widths)
  
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] = as.list(maxWidth)
  }
  
  args = c(grobs, list(ncol=1))
  if (!is.null(heights)) {
    args = c(args, list(heights=heights))
  }
  
  if (plot) {
    # display the plot immediately
    do.call(grid.arrange, args)
  } else {
    # return an arrangeGrob
    return(do.call(arrangeGrob, args))
  }
  
}