#' predictglmnetRatser
#'
#' This function is to predict the glmnet model object on raster data. It accepts
#' the quadratic transformation if it was provided in the original model fit.
#'
#' @param r raster file
#' @param model glmnet model
#' @param slambda the lambda selection
#' @param quadratic logical. If the model was fitted with quadratic terms
#' @param trainingOriginalData original training data with only the predictor columns
#' @param factors factor variables in the model
#' @param filename output file directory for the predicted map. If provided, the map
#' will be save in the disk.
#'
#' @export
predict_glmnet_raster <- function(r, model, slambda = "lambda.min", quadratic = TRUE, trainingOriginalData = NULL, factors = NULL, filename = NULL){
  require(raster)
  require(glmnet)
  # check the requirements
  if(isTRUE(quadratic) && is.null(trainingOriginalData)){
    stop("With quadratic, the training data should be provided")
  }
  if(is.list(factors)){
    factors <- names(factors)
  }
  if(!is.null(trainingOriginalData)){
    myvars <- names(trainingOriginalData)
    if(!all(names(trainingOriginalData) %in% names(r))){
      stop("The raster and training columns do not match")
    }
    r <- r[[myvars]]
  } else{
    myvars <- names(r)
  }
  # function for quadratic tranformations
  glmnetPoly <- function(mydf, mydf2, col){
    trainCol <- which(names(mydf) == col)
    testCol <- which(names(mydf2) == col)
    # calculate the terms for training
    xbar <- mean(mydf[,trainCol])
    x1 <- mydf[,trainCol] - xbar
    alpha <- sum(x1^3)/sum(x1^2)
    # calculate the terms for testing
    x1_2 <- mydf2@data[,testCol] - xbar
    x2_2 <- x1_2^2 - alpha*x1_2
    myx2 <- data.frame(mx1 = x1_2, mx2 = x2_2)
    colnames(myx2) <- c(paste0(names(mydf)[trainCol], "1"), paste0(names(mydf)[trainCol], "2"))
    final <- mydf2
    final@data <- cbind(myx2, mydf2@data[,-testCol])
    return(final)
  }
  testing <- rasterToPoints(r, spatial = TRUE)
  for(m in myvars){
    if(m %in% factors){
      testing@data[,m] <- as.factor(testing@data[,m])
    } else{
      if(quadratic){
        testing <- glmnetPoly(trainingOriginalData, testing, m)
      }
    }
  }
  testing <- testing[complete.cases(testing@data),]
  cat("Preparation is done... \n")
  testing_sparse <- sparse.model.matrix(~. -1, testing@data)
  testing@data$pred = as.numeric(predict(model, testing_sparse, type = "response", s = slambda))
  cat("Finalising... \n")
  y <- rasterize(testing, r, field = "pred")
  if(!is.null(filename)){
    writeRaster(y, filename)
  }
  return(y)
}


#' predict SVM to Raster
#'
#' This function is to predict the e1071 model object on raster data.
#'
#' @inheritParams predictglmnetRatser
#' @param model svm model from e1071 package
#' @inheritParams predictglmnetRatser
#' @inheritParams predictglmnetRatser
#'
#' @return
#' @export
#'
#' @examples
predict_svm_to_raster <- function(r, model, factors = NULL, filename = NULL){
  require(raster)
  require(e1071)
  d <- rasterToPoints(r, spatial = TRUE)
  if(!is.null(factors)){
    for(i in names(factors)){
      d@data[,i] <- as.factor(d@data[,i])
    }
  }
  cat("Preparation is done... \n")
  p <- predict(model, d@data, probability = TRUE)
  cat("Prediction is done... \n")
  d$pred <- attr(p, "probabilities")[,"1"]
  y <- rasterize(d, r, field = "pred")
  cat("Finalising... \n")
  if(!is.null(filename)){
    writeRaster(y, filename)
  }
  return(y)
}
