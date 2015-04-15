#' Plot rasters in ggplot with greyscale
#' @param x raster
#' @param layer layername
#' @param maxpixels Integer. Maximal number of pixels to sample
#' @param lowColor Character. Color for lowest value
#' @param highColor Character. Color for highest value
#' @param legendName Character. Layer name
#' @param ggObj Logical. Return a ggplot2 object (TRUE) or just the data.frame
#' 
#' @export 
ggRx <- function(x, layer = 1, maxpixels = 5000000, lowColor = "white", highColor = "black", legendName = "Legend", ggObj = TRUE) {    

    drast <- sampleRegular(x[[layer]], maxpixels, asRaster = TRUE)
    df <- data.frame(coordinates(drast), drast[])
    colnames(df) <- c("x", "y", names(x[[layer]]))
    layer <- colnames(df)[3]
  #  df <- melt(df, id.vars = c("x","y"))
    
    if(ggObj) {p <- ggplot(df) + geom_raster(aes_string(x = "x", y = "y", fill = layer)) +
            scale_fill_gradient(low = lowColor, high = highColor, na.value = NA, name = legendName) +
            coord_equal() +
            MAPTHEME
} else {
    p <- df
}
return(p)
}



#' Scalebar for ggplot
#'
#' @param ras Raster*
#' @param len Numeric. Length of scalebar (map units)
#' @param yoff y offset from lower left corner (in %)
#' @param xoff x offset from lower left corner (in %)
#' @param sdist Numeric. Length of ticks
#' @param tdist Numeric. Distance text from ticks.
#' @param size Numeric. Thickness of scalebar
#' @param col Color of scalebar and text
#' @export 
SCALE <- function(ras = NULL, len = 10000, yoff = 0.6, xoff = 0.2, sdist = 800, tdist = 1000, size = 1, col = "white"){
    if(is.null(ras)){
        ex <- new("Extent"
                , xmin = 579765
                , xmax = 668775
                , ymin = -522705
                , ymax = -477735
        )
    } else {
        ex <- extent(ras)
    }
    xoff <- xoff/100
    yoff <- yoff/100
    df <- data.frame(xf = ex@xmin*(1+xoff), xt = ex@xmin*(1+xoff) + len, y = ex@ymin * (1-yoff))
    df <- data.frame(x = rep(ex@xmin*(1+xoff),4) + c(0,0,len,len), y = rep(ex@ymin * (1-yoff),4) - c(sdist,0,0,sdist)) 
    
    dft <- data.frame(x = range(df$x), y = min(df$y) - tdist, lab = paste0(c("","   "), c(0, len/1000), c(""," km"))) 
    
    list(geom_line(data = df, aes(x=x, y=y), size = 2, col = col),
            geom_text(data=dft, aes(x = x, y =y, label =lab), col = col) ) 
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
