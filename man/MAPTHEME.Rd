% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ggToolsAndSettings.r
\docType{data}
\name{MAPTHEME}
\alias{MAPTHEME}
\title{Theme for map plotting}
\format{\preformatted{List of 3
 $ :List of 12
  ..$ axis.title.y     : list()
  .. ..- attr(*, "class")= chr [1:2] "element_blank" "element"
  ..$ axis.title.x     : list()
  .. ..- attr(*, "class")= chr [1:2] "element_blank" "element"
  ..$ axis.text.x      :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : chr "grey30"
  .. ..$ size      : num 9
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ axis.text.y      :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : chr "grey30"
  .. ..$ size      : num 9
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : num 90
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ axis.ticks       :List of 4
  .. ..$ colour  : chr "grey30"
  .. ..$ size    : NULL
  .. ..$ linetype: NULL
  .. ..$ lineend : NULL
  .. ..- attr(*, "class")= chr [1:2] "element_line" "element"
  ..$ axis.ticks.length:Class 'unit'  atomic [1:1] 0.06
  .. .. ..- attr(*, "unit")= chr "cm"
  .. .. ..- attr(*, "valid.unit")= int 1
  ..$ strip.text.x     :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : NULL
  .. ..$ size      : num 8
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ strip.text.y     :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : NULL
  .. ..$ size      : num 8
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : num 270
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ plot.title       :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : NULL
  .. ..$ size      : num 9
  .. ..$ hjust     : num 0
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ legend.title     :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : NULL
  .. ..$ size      : num 9
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ legend.text      :List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : NULL
  .. ..$ size      : num 8
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..$ plot.margin      :Class 'unit'  atomic [1:4] 0 0 0 0
  .. .. ..- attr(*, "unit")= chr "lines"
  .. .. ..- attr(*, "valid.unit")= int 3
  ..- attr(*, "class")= chr [1:2] "theme" "gg"
  ..- attr(*, "complete")= logi FALSE
 $ :List of 17
  ..$ call        : language continuous_scale(aesthetics = c("x", "xmin", "xmax", "xend", "xintercept"),      scale_name = "position_c", palette = identity, expand = expand, guide = "none")
  ..$ aesthetics  : chr [1:5] "x" "xmin" "xmax" "xend" ...
  ..$ scale_name  : chr "position_c"
  ..$ palette     :function (x)  
  ..$ range       :Reference class 'Continuous' [package "scales"] with 1 fields
  .. ..$ range: NULL
  .. ..and 15 methods, of which 3 are possibly relevant:
  .. ..  initialize, reset, train
  ..$ limits      : NULL
  ..$ trans       :List of 6
  .. ..$ name     : chr "identity"
  .. ..$ transform:function (x)  
  .. ..$ inverse  :function (x)  
  .. ..$ breaks   :function (x)  
  .. ..$ format   :function (x)  
  .. ..$ domain   : num [1:2] -Inf Inf
  .. ..- attr(*, "class")= chr "trans"
  ..$ na.value    : num NA
  ..$ expand      : num [1:2] 0 0
  ..$ rescaler    :function (x, to = c(0, 1), from = range(x, na.rm = TRUE))  
  ..$ oob         :function (x, range = c(0, 1), only.finite = TRUE)  
  ..$ name        : NULL
  ..$ breaks      : list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ minor_breaks: list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ labels      : list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ legend      : NULL
  ..$ guide       : chr "none"
  ..- attr(*, "class")= chr [1:3] "position_c" "continuous" "scale"
 $ :List of 17
  ..$ call        : language continuous_scale(aesthetics = c("y", "ymin", "ymax", "yend", "yintercept",      "ymin_final", "ymax_final"), scale_name = "position_c", palette = identity,  ...
  ..$ aesthetics  : chr [1:7] "y" "ymin" "ymax" "yend" ...
  ..$ scale_name  : chr "position_c"
  ..$ palette     :function (x)  
  ..$ range       :Reference class 'Continuous' [package "scales"] with 1 fields
  .. ..$ range: NULL
  .. ..and 15 methods, of which 3 are possibly relevant:
  .. ..  initialize, reset, train
  ..$ limits      : NULL
  ..$ trans       :List of 6
  .. ..$ name     : chr "identity"
  .. ..$ transform:function (x)  
  .. ..$ inverse  :function (x)  
  .. ..$ breaks   :function (x)  
  .. ..$ format   :function (x)  
  .. ..$ domain   : num [1:2] -Inf Inf
  .. ..- attr(*, "class")= chr "trans"
  ..$ na.value    : num NA
  ..$ expand      : num [1:2] 0 0
  ..$ rescaler    :function (x, to = c(0, 1), from = range(x, na.rm = TRUE))  
  ..$ oob         :function (x, range = c(0, 1), only.finite = TRUE)  
  ..$ name        : NULL
  ..$ breaks      : list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ minor_breaks: list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ labels      : list()
  .. ..- attr(*, "class")= chr "waiver"
  ..$ legend      : NULL
  ..$ guide       : chr "none"
  ..- attr(*, "class")= chr [1:3] "position_c" "continuous" "scale"
}}
\usage{
MAPTHEME
}
\description{
Theme for map plotting
}
\keyword{datasets}

