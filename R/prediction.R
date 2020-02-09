#' predict cv.glmnet model to ratser
#'
#' This function is to predict the glmnet model object on raster data. It accepts
#' the quadratic transformation if it was provided in the original model fit.
#'
#' @param r a raster file
#' @param model cv.glmnet model object
#' @param slambda the values of the penalty lambda in cv.glmnet model.
#' The default if "lambda.1se", alternatively "lambda.min" can be used.
#' See predict.cv.glment help file in the glmnet pakage.
#' @param factors factor variables in the model
#' @param filename output file directory for the predicted map. If provided, the map
#' will be save on the disk.
#' @param type the prediction type. Could be "link" or "response". The default is "response".
#' @param quadraticObj a make_quadratic object. If the model is fitted with the
#' quadratic terms created by make_quadratic function, provide the make_quadratic object here.
#' @param verbose logical. Control amount of printing...
#'
#' @author Roozbeh Valavi
#'
#' @export
predict_glmnet_raster <- function(r,
                                  model,
                                  slambda = "lambda.1se",
                                  type = "response",
                                  quadraticObj = NULL,
                                  factors = NULL,
                                  filename = NULL,
                                  verbose = TRUE){
  require(glmnet)
  # check the requirements
  if(is.list(factors)){
    factors <- names(factors)
  }
  if(!methods::is(r, "Raster")) stop("r should be a raster object.")
  myvars <- names(r)
  # if(!is.null(quadraticObj)){
  #   r <- predict.make_quadratic(object = quadraticObj, newdata = r)
  # }
  testing <- raster::rasterToPoints(r, spatial = TRUE)
  for(m in myvars){
    if(m %in% factors){
      testing@data[,m] <- as.factor(testing@data[,m])
    }
  }
  if(!is.null(quadraticObj)){
    testing@data <- predict.make_quadratic(object = quadraticObj, newdata = testing@data)
  }
  testing <- testing[stats::complete.cases(testing@data),]
  if(verbose) cat("Preparation is done... \n")
  data_sparse <- sparse.model.matrix(~. -1, testing@data)
  testing@data$pred = as.numeric(predict(model, data_sparse, type = type, s = slambda))
  if(verbose) cat("Finalising... \n")
  y <- raster::rasterize(testing, r, field = "pred")
  if(!is.null(filename)){
    raster::writeRaster(y, filename)
  }
  return(y)
}


#' predict SVM to Raster
#'
#' This function is to predict the e1071 model object on raster data.
#'
#' @inheritParams predict_glmnet_raster
#' @param model svm model from e1071 package
#' @inheritParams predict_glmnet_raster
#' @inheritParams predict_glmnet_raster
#'
#' @return a raster file for prediction
#' @export
#'
#' @author Roozbeh Valavi
#'
#' @examples
predict_svm_raster <- function(r, model, factors = NULL, filename = NULL){
  require(raster)
  require(e1071)
  d <- raster::rasterToPoints(r, spatial = TRUE)
  if(!is.null(factors)){
    for(i in names(factors)){
      d@data[,i] <- as.factor(d@data[,i])
    }
  }
  cat("Preparation is done... \n")
  p <- predict(model, d@data, probability = TRUE)
  cat("Prediction is done... \n")
  d$pred <- attr(p, "probabilities")[,"1"]
  y <- raster::rasterize(d, r, field = "pred")
  cat("Finalising... \n")
  if(!is.null(filename)){
    raster::writeRaster(y, filename)
  }
  return(y)
}
