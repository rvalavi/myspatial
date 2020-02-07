#' Degitizing points on a spatial plot
#'
#' @param sp logical. To return spatial points
#' @param col the color to plot points with
#' @param ...
#'
#' @author Roozbeh Valavi
#'
#' @return
#' @export
#'
#' @examples
makePoints <- function(sp = TRUE, col = "red", ...){
  xy <- graphics::locator(n = 10000, type = "p", col = col, ...)
  xy <- cbind(xy$x, xy$y)
  if(sp){
    return(SpatialPoints(xy))
  }
  else {
    return(xy)
  }
}
