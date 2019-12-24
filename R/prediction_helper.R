#' Orthogonal quadratic polynomials for glmnet
#'
#' A function to creat quadratic terms for lasso and ridge
#'
#' @param df The data.frame of training covariates
#' @param cols Character. Column names to generate quadratic terms
#' @param all If TRUE, all the columns are used to generate quadratic terms
#'
#' @return A data.frame with polynomial terms of the original dataset.
#' @export
#'
#' @examples
quadratic_for_glmnet <- function(df, cols = NULL, all = TRUE){
  if(is.null(cols)){
    all <- TRUE
    cols <- colnames(df)
  }
  for(j in cols){
    if(is.factor(df[,j])){
      cols <- cols[which(cols != j)]
      warning("Column '", j , "' is a factor, so it is excluded from quadratic columns.")
    }
  }
  for(i in cols){
    dfCol <- which(names(df) == i)
    xbar <- mean(df[, dfCol])
    x1 <- df[, dfCol] - xbar
    alpha <- sum(x1 ^ 3) / sum(x1 ^ 2)
    x2 <- x1 ^ 2 - alpha * x1
    myx1 <- data.frame(mx1 = x1, mx2 = x2)
    colnames(myx1) <- c(paste0(names(df)[dfCol], "1"), paste0(names(df)[dfCol], "2"))
    df <- cbind(myx1, df[, -dfCol])
  }
  return(df)
}
