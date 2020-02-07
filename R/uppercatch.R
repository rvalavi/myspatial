#' Extract and summarise raster values for upper-catchments
#'
#'
#' @param ccm2 the CCM2 spatil polygon layer of subcatchments
#' @param r a Raster later or RasterStack for extracting values
#' @param myfun a function to summarise the values of input raster e.g. mean or sum
#'
#' @author Roozbeh Valavi
#'
#' @return
#' @export
#'
#' @examples
upperCatchment <- function(ccm2, r, myfun = "mean"){
  require(progress)
  require(raster)
  pb <- progress::progress_bar$new(
    format = " Progress [:bar] :percent in :elapsed",
    total = nrow(ccm2), clear = FALSE, width = 75) # add progress bar
  if(is.factor(ccm2$WSO1_ID)){
    ccm2$WSO1_ID <- as.numeric(as.character(ccm2$WSO1_ID))
  }
  if(is.factor(ccm2$NEXTDOWNID)){
    ccm2$NEXTDOWNID <- as.numeric(as.character(ccm2$NEXTDOWNID))
  }
  for(i in seq_len(nrow(ccm2))){
    catch <- ccm2$WSO1_ID[i]
    fc <- vector(mode = "integer")
    n <- 1
    while(n > 0){
      s <- which(ccm2$NEXTDOWNID %in% catch)
      catch <- ccm2$WSO1_ID[s]
      fc <- append(fc, catch)
      n <- length(s)
    }
    subcatchs <- raster::aggregate(ccm2[ccm2$WSO1_ID %in% c(ccm2$WSO1_ID[i], fc), ])
    val <- raster::extract(r, subcatchs, fun = get(myfun))
    ccm2[i, paste0(myfun, "_", names(r))] <- val
    pb$tick() # update progress bar
  }
  return(ccm2)
}
