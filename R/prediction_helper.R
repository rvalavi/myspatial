#' Orthogonal polynomial for glmnet
#'
#' A function to creat quadratic terms for lasso and ridge
#'
#' @param train training data.frame
#' @param cols Character. Column names to generate quadratic terms
#' @param all If TRUE, all the columns are used to generate quadratic terms
#'
#' @return A data.frame with polynomial terms of the original dataset.
#' @export
#'
#' @examples
glmnetPoly <- function(train, cols = NULL, all = TRUE){
  if(all){
    cols <- colnames(train)
  }
  for(i in cols){
    trainCol <- which(names(train) == i)
    # generating quadratic terms for training data
    xbar <- mean(train[, trainCol])
    x1 <- train[, trainCol] - xbar
    alpha <- sum(x1 ^ 3) / sum(x1 ^ 2)
    x2 <- x1 ^ 2 - alpha * x1
    myx1 <- data.frame(mx1 = x1, mx2 = x2)
    colnames(myx1) <- c(paste0(names(train)[trainCol], "1"), paste0(names(train)[trainCol], "2"))
    train <- cbind(myx1, train[, -trainCol])
  }
  return(train)
}
