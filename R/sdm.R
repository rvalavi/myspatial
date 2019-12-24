#' Calculating species range change
#'
#' The values of raster show:
#' 0 = unsiitable
#' 1 = loss
#' 2 = stable
#' 3 = gain
#'
#' @param current the current raster map
#' @param future the future raster map
#' @param threshold optional. A threshold to change probability map to 0 and 1
#'
#' @return
#' @export
#'
#' @examples
rangeChange <- function(current, future, threshold = NULL){
  if(!is.null(threshold)){
    current <- raster::reclassify(current, c(-Inf,threshold,0, threshold,Inf,1))
    future <- raster::reclassify(future, c(-Inf,threshold,0, threshold,Inf,1))
  }
  rr <- raster::overlay(current, future, fun=function(x, y){x + y * 2})
  tab <- as.data.frame(table(raster::values(rr)))
  val <- c("Current range", "Future range", "Unsuitable",
           "Loss", "Gain", "Stable", "Percent loss (%)",
           "Percent gain (%)", "Range change", "Percent change (%)")
  change <- data.frame(Category = val, Value = NA)
  change$Value[1] <- sum(tab$Freq[c(2,4)])
  change$Value[2] <- sum(tab$Freq[c(3,4)])
  change$Value[3] <- tab$Freq[1]
  change$Value[4] <- tab$Freq[2]
  change$Value[5] <- tab$Freq[3]
  change$Value[6] <- tab$Freq[4]
  change$Value[7] <- change$Value[4] / change$Value[1] * 100
  change$Value[8] <- change$Value[5] / change$Value[1] * 100
  change$Value[9] <- sum(tab$Freq[c(3,4)]) - sum(tab$Freq[c(2,4)])
  change$Value[10] <- change$Value[9] / sum(tab$Freq[c(2,4)]) * 100
  change$Value <- round(change$Value, 2)
  names(rr) <- "range_change"
  rr <- raster::as.factor(rr)
  rat <- data.frame(ID = c(0:3))
  rat$range <- c("Unsutable", "Loss", "Gain", "Stable")
  rat$code <- c(0, -1, 1, 2)
  levels(rr) <- rat
  cat("The range change (values are nummber of pixels):\n")
  print(change)
  plot(rasterVis::levelplot(rr, par.settings = rasterVis::rasterTheme(viridis::viridis(4, direction = -1))))
  return(rr)
}

