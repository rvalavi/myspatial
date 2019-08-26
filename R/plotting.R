#' rasterPlot
#'
#' Plot probability raster of species distribution models in ggplot,
#' with histogram of the values.
#'
#' @param map raster of SDM probability
#' @param cols a string vector containing the colours code
#' @param legend logical. to show legend or not.
#'
#' @return
#' @export
#'
#' @examples
rasterPlot <- function(map, cols = terrain.colors(10), legend = TRUE){
  require(ggplot2)
  require(raster)
  require(grid)
  if(ncell(map) <= 5e5){
    samp <- map
  } else{
    samp <- raster::sampleRegular(map, 5e+05, asRaster = TRUE)
  }
  # make the sampled raster a dataframe for ggplot
  df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, na.rm = TRUE)
  # make appropriate column headings
  colnames(df) <- c("Longitude", "Latitude", "Likelihood")
  p1 <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Likelihood)) +
    scale_fill_gradientn(colours = cols,
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1),
                         limits = c(0, 1)) +
    theme_minimal()  +
    coord_fixed() +
    theme(text = element_text(family = "Times New Roman", size = 12),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.82, 0.23)) +
    guides(fill = guide_colorbar(barwidth = 1,
                                 barheight = 6))
  if(legend == FALSE){
    p1 <- p1 + guides(fill = FALSE)
  }
  # create histogram
  p2 <- ggplot(data = df, aes(x = Likelihood)) +
    geom_histogram(bins = 50, fill = cols) +
    expand_limits(x = c(0, 1)) +
    theme_classic() +
    theme(panel.background = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    xlab("") + ylab("") +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  # create viewpoint and combine plots
  vp <- viewport(width = 0.31,
                 height = 0.33,
                 x = 0.37,
                 y = 0.7)
  full <- function(){
    print(p1)
    theme_set(theme_bw(base_size = 8))
    print(p2, vp = vp)
    theme_set(theme_bw())
  }
  full()
}
