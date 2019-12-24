ggCorRaster <- function(r, nsam = 1e4){
  require(ggplot2)
  samxy <- dismo::randomPoints(r, nsam)
  vals <- as.data.frame(raster::extract(r, samxy))
  plt <- GGally::ggcorr(vals,
                method = c("pairwise", "spearman"),
                label = TRUE,
                label_size = 4,
                label_color = "black",
                digits = 3) +
    theme(axis.text = element_text(margin = margin(r = 10, l =10)),
          legend.justification = c(1, 0),
          legend.position = c(0.5, 0.7),
          legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 9, 
                                 barheight = 1, 
                                 title.position = "top", 
                                 title.hjust = 0.5, 
                                 title = "Spearman correlation"))
  plot(plt)
  return(plt)
}
