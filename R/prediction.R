predictglmnetRatser <- function(r, model, slambda = "lambda.min", quadratic = TRUE, trainingOriginalData = NULL, factors = NULL, filename = NULL){
  require(raster)
  require(glmnet)
  # check the requirements
  if(isTRUE(quadratic) && is.null(trainingOriginalData)){
    stop("With quadratic, the training data should be provided")
  }
  if(is.list(factors)){
    factors <- names(factors)
  }
  myvars <- names(trainingOriginalData)
  if(!all(names(trainingOriginalData) %in% names(r))){
    stop("The raster and training columns do not match")
  }
  r <- r[[myvars]]
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
    if(m %in% factors == FALSE){
      testing <- glmnetPoly(trainingOriginalData, testing, m)
    } else{
      testing@data[,m] <- as.factor(testing@data[,m])
    }
  }
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
