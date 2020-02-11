#' Orthogonal quadratic polynomials for glmnet
#'
#' A function to creat quadratic terms for glmnet functions i.e. lasso and ridge regression.
#' The output is an object of make_quadratic that can be used to predict on rasters and data.frames
#' for creating the quadratic terms.
#'
#' @param df a data.frame, typically the training data.
#' @param cols the name or index of the columns to be transformed. If NULL, all the columns will be transformed.
#' The factor columns won't be transfromed.
#'
#' @author Roozbeh Valavi
#'
#' @return an object of make_quadratic that can be used to predict on rasters and data.frames
#' @export
#'
#' @examples
make_quadratic <- function(df, cols = NULL){
  if(is.null(cols)){
    cols <- colnames(df)
  }
  if(is.numeric(cols)){
    cols <- colnames(df)[cols]
  }
  # remove the factors
  if(any(sapply(df[,cols], is.factor))){
    message("The factor columns were removed form cols: ", cols[which(sapply(df[,cols], is.factor))])
    cols <- cols[-which(sapply(df[,cols], is.factor))]
  }
  if(!all(is.element(cols, colnames(df)))){
    stop("The cols should be the same as the column names.")
  }
  xbar <- apply(df[,cols], 2, mean)
  x1 <- data.frame(mapply(`-`, df[,cols], xbar, SIMPLIFY = FALSE))
  alpha <- colSums(x1 ^ 3) / colSums(x1 ^ 2)
  # specify the output class
  finalList <- list(names = cols, xbars = xbar, alphas = alpha)
  class(finalList) <- c("make_quadratic")
  return(finalList)
}


#' @export
#' @method predict make_quadratic
predict.make_quadratic <- function(object, newdata, ...){
  if(!methods::is(object, "make_quadratic"))
    stop("object should be a make_quadratic object.")
  if(!all(object$names %in% names(newdata)))
    stop("The newdata does not have the same names as the object.")
  ncl <- object$names
  if(methods::is(newdata, "Raster")){
    for(i in ncl){
      x1 <- newdata[[i]] - object$xbars[i]
      x2 <- (x1 ^ 2) - (object$alphas[i] * x1)
      if(raster::nlayers(newdata) > 1){
        newdata <- newdata[[-which(names(newdata) == i)]]
        newdata <- raster::stack(newdata, x1)
      } else{
        newdata <- x1
      }
      names(newdata)[raster::nlayers(newdata)] <- paste0(i, "_1")
      newdata <- raster::stack(newdata, x2)
      names(newdata)[raster::nlayers(newdata)] <- paste0(i, "_2")
    }
  } else if(methods::is(newdata, "data.frame")){
    for(i in ncl){
      x1 <- newdata[,i] - object$xbars[i]
      x2 <- x1 ^ 2 - object$alphas[i] * x1
      newdata <- newdata[,-which(names(newdata) == i)]
      newdata[,ncol(newdata) + 1] <- x1
      names(newdata)[ncol(newdata)] <- paste0(i, "_1")
      newdata[,ncol(newdata) + 1] <- x2
      names(newdata)[ncol(newdata)] <- paste0(i, "_2")
    }
  } else stop("newdata should be a raster or a data.frame.")
  return(newdata)
}

