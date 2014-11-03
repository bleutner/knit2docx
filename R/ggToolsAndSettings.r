#' Plot rasters in ggplot with greyscale
#' @param x raster
#' @param layer layername
#' @param maxpixels Integer. Maximal number of pixels to sample
#' @param lowColor Character. Color for lowest value
#' @param highColor Character. Color for highest value
#' @param legendName Character. Layer name
#' 
#'  @export 
ggR <- function(x, layer = 1, maxpixels = 5000000, lowColor = "white", highColor = "black", legendName = "Legend") {  
    drast <- sampleRegular(x[[layer]], maxpixels, asRaster = TRUE)
    df <- data.frame(coordinates(drast), drast[])
    colnames(df) <- c("x", "y", names(x[[layer]]))
    layer <- colnames(df)[3]
  #  df <- melt(df, id.vars = c("x","y"))
    
    ggplot(df) + geom_raster(aes_string(x = "x", y = "y", fill = layer)) +
            scale_fill_gradient(low = lowColor, high = highColor, na.value = NA, name = legendName) +
            coord_equal() +
            MAPTHEME
}

#' Theme for map plotting
#' @export 
MAPTHEME <- theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=8, colour="grey50"),
        axis.text.y  = element_text(angle=90, size=8, colour="grey50"),
        axis.ticks    = element_line(colour = "grey50"),
        axis.ticks.length = unit(0.06, "cm"),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle=270),
        plot.title   = element_text(size = 9, hjust  =  0),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8)				
)
