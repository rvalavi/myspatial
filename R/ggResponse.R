#' Partial dependence plot by ggplot
#'
#' This function can be used to plot a partial dependence plot for any model that
#' can predict on data.frames.
#'
#' @param models a model object
#' @param covariates the covariates used in model fitting, a raster or data.frame
#' @param colPlot integer. The number of colums for plotting
#' @param responseName character. the name for y axes
#' @param index integer. The columns used for prediction. This relates to the factor level for classification.
#' @param ... other arguments e.g. type = 'response' in GLMs or type = 'prob' in randomForest or the number of trees in BRT/GBM.
#'
#' @author Roozbeh Valavi
#'
#' @export
#'
#' @examples
ggResponse <- function(models,
                       covariates,
                       colPlot = 3,
                       responseName = "Prediction",
                       index = 2, ...){
  require(raster)
  require(dplyr)
  require(reshape)
  require(tidyverse)
  require(cowplot)
  n <- 0
  categoricals <- c()
  if(is(covariates, "Raster")){
    nlayer <- raster::nlayers(covariates)
    meanVars <- matrix(nrow=100, ncol=nlayer)
    meanVars <- as.data.frame(meanVars)
    names(meanVars) <- names(covariates)
    ranges <- predictions <- meanVars
    categoricals <- names(covariates)[which(is.factor(covariates))]
    if(anyNA(maxValue(covariates))){
      naminmax <- which(is.na(maxValue(covariates)))
      for(l in naminmax){
        covariates[[l]] <- raster::setMinMax(covariates[[l]])
      }
    }
    # calculate the means and ranges for non-categorical vars
    for(i in 1:nlayer){
      if(!is.factor(covariates[[i]])){
        ranges[,i] <- seq(minValue(covariates[[i]]), maxValue(covariates[[i]]), length.out = 100)
        meanVars[,i] <- rep(mean(values(covariates[[i]]), na.rm=TRUE), 100)
      }
    }
  } else if(is(covariates, "data.frame")){
    nlayer <- ncol(covariates)
    meanVars <- matrix(nrow=100, ncol=nlayer)
    meanVars <- as.data.frame(meanVars)
    names(meanVars) <- names(covariates)
    ranges <- predictions <- meanVars
    for(b in 1:nlayer){
      if(is.factor(covariates[,b])){
        n <- n + 1
        categoricals[n] <- names(covariates)[b]
      }
    }
    # calculate the means and ranges for non-categorical vars
    for(i in 1:nlayer){
      if(!is.factor(covariates[[i]])){
        ranges[,i] <- seq(min(covariates[,i], na.rm = TRUE), max(covariates[,i], na.rm = TRUE), length.out = 100)
        meanVars[,i] <- rep(mean(covariates[, i, drop = TRUE], na.rm = TRUE), 100)
      }
    }
  } else{
    stop("covariates should be a raster layer or data.frame object contining variables used in the model")
  }
  # calculate the means and ranges for categorical vars
  if(length(categoricals) > 0){
    cats <- which(names(covariates) %in% categoricals) # categorical vars
    if(is(covariates, "data.frame")){
      for(ct in cats){
        commCats <- names(which(table(covariates[,ct]) == max(table(covariates[,ct]))))
        level <- unlist(levels(covariates[,ct]))
        ranges[,ct] <- c(level, sample(level, 100 - length(level), replace = T))
        meanVars[,ct] <- rep(commCats, 100)
        ranges[,ct] <- as.factor(ranges[,ct])
        meanVars[,ct] <- as.factor(meanVars[,ct])
      }
    } else{
      for(ct in cats){
        commCats <- names(which(table(values(covariates[[ct]])) == max(table(values(covariates[[ct]])))))
        level <- unlist(levels(covariates[[ct]]))
        ranges[,ct] <- c(level, sample(level, 100 - length(level), replace = T))
        meanVars[,ct] <- rep(commCats, 100)
        ranges[,ct] <- as.factor(ranges[,ct])
        meanVars[,ct] <- as.factor(meanVars[,ct])
      }
    }
  }
  # predict with the model
  for(j in 1:nlayer){
    mydf <- cbind(ranges[,j, drop = FALSE], meanVars[,-j, drop = FALSE])
    names(mydf)[1] <- colnames(meanVars)[j]
    for(c in categoricals){
      if(is(covariates, "Raster")){
        levels(mydf[, c]) <- as.character(unlist(levels(covariates[[c]])))
      } else{
        levels(mydf[, c]) <- levels(covariates[,c])
      }
    }
    # browser()
    pred <- predict(models, mydf, ...)
    if(length(dim(pred)) > 1){
      predictions[,j] <- pred[,index]
    } else{
      predictions[,j] <- pred
    }
    # if(!is.vector(pred) && ncol(pred) > 1){
    #   predictions[,j] <- pred[,index]
    # } else{
    #   predictions[,j] <- pred
    # }
  }
  # change the cats to numeric for melting
  if(length(categoricals) > 0){
    for(ct in cats){
      ranges[,ct] <- as.numeric(as.character(ranges[,ct]))
      predictions[,ct] <- as.numeric(as.character(predictions[,ct]))
    }
  }
  val <- reshape::melt(ranges)
  # nrow(val);head(val, 10)
  prd <- reshape::melt(predictions)
  # nrow(prd); head(prd, 10)
  finaltable <- dplyr::bind_cols(val, prd)
  yMin <- min(finaltable$value1)
  yMax <- max(finaltable$value1)
  # create the plots
  pp <- list()
  for(k in 1:nlayer){
    down <- k*100-99
    up <- k*100
    pp[[k]] <- ggplot(data=finaltable[down:up,], aes(x=value, y=value1)) + geom_line() +
      xlab(stringr::str_to_upper(names(covariates)[k])) + scale_y_continuous(name=responseName, limits = c(yMin, yMax)) +
      theme_bw()
  }
  # create plot for categorical variables
  if(length(categoricals) > 0){
    for(ct in cats){
      lutable <- finaltable[which(finaltable$variable == names(covariates)[ct]),]
      lutable$value <- as.factor(lutable$value)
      catText <- "ggplot(data=lutable, aes(x=value, y=value1)) + geom_point(size=0.1) +
      xlab(stringr::str_to_sentence(names(covariates)[ct])) + scale_y_continuous(name=responseName, limits = c(yMin, yMax)) + theme_bw() +"
      for(i in 1:length(level)){
        n <- n + 1
        if(i < length(level)){
          tmp <- sprintf("geom_segment(aes(x=(%d - 0.5),xend=(%d + 0.5),y=lutable[%d,'value1'],yend=lutable[%d,'value1']), size=1.2) +", i,i,i,i)
        } else{
          tmp <- sprintf("geom_segment(aes(x=(%d - 0.5),xend=(%d + 0.5),y=lutable[%d,'value1'],yend=lutable[%d,'value1']), size=1.2)", i,i,i,i)
        }
        catText <- paste(catText, tmp)
      }
      pp[[ct]] <- eval(parse(text = catText))
    }
  }
  # create final plot
  cowplot::plot_grid(plotlist = pp, ncol = colPlot)
  # return(pp)
}


#' Partial dependence plot by ggplot with 95 percent confidence interval
#'
#' This function can be used to plot a partial dependence plot for any model that can predict on data.frames. This function calculates the 95 percent confidence interval created by multiple models (from cross-validation, bootstrapping or multi-model) around each curve.
#'
#' @param models a list of model objects (several fitted models on the same dataset).
#' @inheritParams ggResponse
#'
#' @export
#'
#' @author Roozbeh Valavi
#'
#' @examples
ggResponse2 <- function(models,
                        covariates,
                        colPlot = 3,
                        responseName = "Prediction",
                        index = 2,...){
  require(raster)
  require(cowplot)
  require(tidyverse)
  require(reshape)
  nmodel <- 1
  if(is(models, "list")){
    nmodel <- length(models)
  }
  n <- 0
  categoricals <- c()
  if(is(covariates, "Raster")){
    nlayer <- raster::nlayers(covariates)
    meanVars <- matrix(nrow=100, ncol=nlayer)
    meanVars <- as.data.frame(meanVars)
    names(meanVars) <- names(covariates)
    ranges <- predictions <- meanVars
    categoricals <- names(covariates)[which(is.factor(covariates))]
    if(anyNA(raster::maxValue(covariates))){
      naminmax <- which(is.na(raster::maxValue(covariates)))
      for(l in naminmax){
        covariates[[l]] <- raster::setMinMax(covariates[[l]])
      }
    }
    # calculate the means and ranges for non-categorical vars
    for(i in 1:nlayer){
      if(!is.factor(covariates[[i]])){
        ranges[,i] <- seq(minValue(covariates[[i]]), maxValue(covariates[[i]]), length.out = 100)
        meanVars[,i] <- rep(mean(values(covariates[[i]]), na.rm=TRUE), 100)
      }
    }
  } else if(is(covariates, "data.frame")){
    nlayer <- ncol(covariates)
    meanVars <- matrix(nrow=100, ncol=nlayer)
    meanVars <- as.data.frame(meanVars)
    names(meanVars) <- names(covariates)
    ranges <- predictions <- meanVars
    for(b in 1:nlayer){
      if(is.factor(covariates[,b])){
        n <- n + 1
        categoricals[n] <- names(covariates)[b]
      }
    }
    # calculate the means and ranges for non-categorical vars
    for(i in 1:nlayer){
      if(!is.factor(covariates[[i]])){
        ranges[,i] <- seq(min(covariates[,i]), max(covariates[,i]), length.out = 100)
        meanVars[,i] <- rep(mean(covariates[,i, drop = TRUE]), 100)
      }
    }
  } else{
    stop("covariates should be a raster layer or data.frame object contining variables used in the model")
  }
  # browser()
  # calculate the means and ranges for categorical vars
  if(length(categoricals) > 0){
    cats <- which(names(covariates) %in% categoricals) # categorical vars
    if(is(covariates, "data.frame")){
      for(ct in cats){
        commCats <- names(which(table(covariates[,ct]) == max(table(covariates[,ct]))))
        level <- unlist(levels(covariates[,ct]))
        ranges[,ct] <- c(level, sample(level, 100 - length(level), replace = T))
        meanVars[,ct] <- rep(commCats, 100)
        ranges[,ct] <- as.factor(ranges[,ct])
        meanVars[,ct] <- as.factor(meanVars[,ct])
      }
    } else{
      for(ct in cats){
        commCats <- names(which(table(values(covariates[[ct]])) == max(table(values(covariates[[ct]])))))
        level <- unlist(levels(covariates[[ct]]))
        ranges[,ct] <- c(level, sample(level, 100 - length(level), replace = T))
        meanVars[,ct] <- rep(commCats, 100)
        ranges[,ct] <- as.factor(ranges[,ct])
        meanVars[,ct] <- as.factor(meanVars[,ct])
      }
    }
  }
  pred_list <- vector(mode = "list", length = nmodel)
  if(nmodel > 1){
    for(m in 1:nmodel){
      # predict with the model
      for(j in 1:nlayer){
        mydf <- cbind(ranges[,j, drop = FALSE], meanVars[,-j, drop = FALSE])
        names(mydf)[1] <- colnames(meanVars)[j]
        for(c in categoricals){
          if(is(covariates, "Raster")){
            levels(mydf[, c]) <- as.character(unlist(levels(covariates[[c]])))
          } else{
            levels(mydf[, c]) <- levels(covariates[,c])
          }
        }
        pred <- predict(models[[m]], mydf, ...)
        # if(!is.vector(pred) && ncol(pred) > 1){
        #   predictions[,j] <- pred[,index]
        # } else{
        #   predictions[,j] <- pred
        # }
        if(length(dim(pred)) > 1){
          predictions[,j] <- pred[,index]
        } else{
          predictions[,j] <- pred
        }
      }
      if(length(categoricals) > 0){
        for(ct in cats){
          predictions[,ct] <- as.numeric(as.character(predictions[,ct]))
        }
      }
      pred_list[[m]] <- predictions
    }
  } else{
    for(j in 1:nlayer){
      mydf <- cbind(ranges[,j], meanVars[,-j])
      names(mydf)[1] <- colnames(meanVars)[j]
      for(c in categoricals){
        if(is(covariates, "Raster")){
          levels(mydf[, c]) <- as.character(unlist(levels(covariates[[c]])))
        } else{
          levels(mydf[, c]) <- levels(covariates[,c])
        }
      }
      pred <- predict(models[[m]], mydf, ...)
      if(!is.vector(pred) && ncol(pred) > 1){
        predictions[,j] <- pred[,index]
      } else{
        predictions[,j] <- pred
      }
      # predictions[,j] <- predict(models, mydf, ...) ## need to be modified according to the model
    }
    if(length(categoricals) > 0){
      for(ct in cats){
        predictions[,ct] <- as.numeric(as.character(predictions[,ct]))
      }
    }
  }
  if(nmodel > 1){
    # change the cats to numeric for melting
    if(length(categoricals) > 0){
      for(ct in cats){
        ranges[,ct] <- as.numeric(as.character(ranges[,ct]))
      }
    }
    val <- reshape::melt(ranges)
    # nrow(val);head(val, 10)
    prd <- pred_list %>%
      map(reshape::melt) %>%
      do.call(cbind, .) %>%
      purrr::set_names(paste0("col", 1:ncol(.))) %>%
      dplyr::select(c("col1", names(dplyr::select_if(., is.numeric)))) %>%
      dplyr::mutate(mean_pred = apply(dplyr::select_if(., is.numeric), 1, mean),
                    lowInf = mean_pred - 2 * (apply(dplyr::select_if(., is.numeric), 1, sd) / sqrt(nmodel)),
                    uppInf = mean_pred + 2 * (apply(dplyr::select_if(., is.numeric), 1, sd) / sqrt(nmodel)))
    finaltable <- dplyr::bind_cols(val, prd)
    yMin <- min(finaltable$lowInf)
    yMax <- max(finaltable$uppInf)
  } else{
    # change the cats to numeric for melting
    if(length(categoricals) > 0){
      for(ct in cats){
        ranges[,ct] <- as.numeric(as.character(ranges[,ct]))
        predictions[,ct] <- as.numeric(as.character(predictions[,ct]))
      }
    }
    val <- reshape::melt(ranges)
    prd <- reshape::melt(predictions)
    finaltable <- dplyr::bind_cols(val, prd)
    names(finaltable)[4] <- "mean_pred"
    yMin <- min(finaltable$mean_pred)
    yMax <- max(finaltable$mean_pred)
  }
  # create the plots
  pp <- list()
  if(nmodel > 1){
    for(k in 1:nlayer){
      down <- k*100-99
      up <- k*100
      pp[[k]] <- ggplot(data=finaltable[down:up,], aes(x=value, y=mean_pred)) +
        geom_ribbon(aes(ymin = lowInf, ymax = uppInf), fill = "grey70", alpha = 0.6) + geom_line() +
        xlab(toupper(names(covariates)[k])) + scale_y_continuous(name=responseName, limits = c(yMin, yMax)) +
        theme_bw()
    }
  } else{
    for(k in 1:nlayer){
      down <- k*100-99
      up <- k*100
      pp[[k]] <- ggplot(data=finaltable[down:up,], aes(x=value, y=mean_pred)) + geom_line() +
        xlab(toupper(names(covariates)[k])) + scale_y_continuous(name=responseName, limits = c(yMin, yMax)) +
        theme_bw()
    }
  }
  # create plot for categorical variables
  if(length(categoricals) > 0){
    for(ct in cats){
      lutable <- finaltable[which(finaltable$variable == names(covariates)[ct]),]
      lutable$value <- as.factor(lutable$value)
      catText <- "ggplot(data=lutable, aes(x=value, y=mean_pred)) + geom_point(size=0.1) +
      xlab(toupper(names(covariates)[ct])) + scale_y_continuous(name=responseName, limits = c(yMin, yMax)) + theme_bw() +"
      for(i in 1:length(level)){
        n <- n + 1
        if(i < length(level)){
          tmp <- sprintf("geom_segment(aes(x=(%d - 0.5),xend=(%d + 0.5),y=lutable[%d,'mean_pred'],yend=lutable[%d,'mean_pred']), size=1.2) +", i,i,i,i)
        } else{
          tmp <- sprintf("geom_segment(aes(x=(%d - 0.5),xend=(%d + 0.5),y=lutable[%d,'mean_pred'],yend=lutable[%d,'mean_pred']), size=1.2)", i,i,i,i)
        }
        catText <- paste(catText, tmp)
      }
      pp[[ct]] <- eval(parse(text = catText))
    }
  }
  cowplot::plot_grid(plotlist = pp, ncol = colPlot)
  # return(pp)
}
