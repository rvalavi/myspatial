#' Focal extraction from a raster object
#'  function to extract raster values from neighbourhood of points
#'
#' @param r raster layer
#' @param p the spatial point or sf objects
#' @param neighbourhood the dimention of the neighourhood kernel
#' @param fun function to summarize the extracted values (e.g. mean). If
#' no function is provided, a list is retuned
#' @param na.rm logical. If na.rm=TRUE (the default value), NA values are removed before fun is applied.
#' @param progress logical. Show a progress par.
#'
#' @return
#' @export
#'
#' @examples
focalExtract <- function(r, p, neighbourhood = c(3, 3), fun = mean, na.rm = TRUE, progress = TRUE){
  require(raster)
  require(sf)
  require(progress)
  if(is(p, "Spatial")){
    coords <- sp::coordinates(p)
  } else if( is(p, "sf")){
    coords <- sf::st_coordinates(p)
  }
  pb <- progress::progress_bar$new(format = " Progress [:bar] :percent in :elapsed",
                                   total=nrow(coords), clear=FALSE, width=75) # add progress bar
  n <- 0
  if(is.null(fun)){
    output <- vector(mode = "list", length = nrow(coords))
  } else{
    output <- vector(mode = "numeric", length = nrow(coords))
  }
  for(i in 1:nrow(coords)){
    n <- n + 1
    rw <- rowFromY(r, coords[i,2])
    cl <- colFromX(r, coords[i,1])
    my_values <- getValuesFocal(r, row = rw, nrows = 1, ngb = neighbourhood, names = FALSE)[cl,]
    if(is.null(fun)){
      if(isTRUE(na.rm)){
        output[[n]] <- as.vector(na.omit(my_values))
      } else{
        output[[n]] <- my_values
      }
    } else{
      output[n] <- fun(my_values, na.rm = na.rm)
    }
    if(progress == TRUE){
      pb$tick() # update progress bar
    }
  }
  return(output)
}
