##' @title Interactive visualisation of a single spectrum
##'
##' @description
##'
##' The `plotlySpectra()` function displays a single spectrum stored
##' in a `Spectra` object using the `plotly::plot_ly()` function.
##'
##' See `?SpectraVis` for an example.
##'
##' @param object A instance of class `Spectra` of length 1.
##'
##' @return A `plotly` object.
##'
##' @importFrom plotly plot_ly add_segments layout
##'
##' @export
##'
##' @author Laurent Gatto, Johannes Rainer
##'
##' @examples
##' library(msdata)
##' fl <- system.file("TripleTOF-SWATH", "PestMix1_DDA.mzML", package = "msdata")
##' pest_ms2 <- filterMsLevel(Spectra(fl), 2L)
##'
##' plotlySpectra(pest_ms2[950])
plotlySpectra <- function(object) {
    stopifnot(inherits(object, "Spectra"))
    p <- plot_ly()
    p <- layout(p, xaxis = list(title = "m/z"),
                yaxis = list(title = "intensity", zeroline = TRUE),
                hovermode = "x", hoverdistance = 1)
    if (length(object) == 0) return(p)
    if (length(object) != 1)
        stop("Object must be of length 1.")
    x <- data.frame(peaksData(object)[[1]])
    x$zero <- 0
    .plotly_peaks(p, x)
}

#' @param p `plot_ly` instance.
#'
#' @param data `data.frame` with the data to be plotted.
#'
#' @param col colors for the segments. Can be of length 1 or equal to the number
#'     of peaks.
#'
#' @param name `character(1)` defining the name of the spectrum.
#'
#' @author Johannes Rainer
#' 
#' @noRd
.plotly_peaks <- function(p, data, col = "#737373", colors = col, name = "") {
    add_segments(p, data = data, x = ~mz, y = ~zero, xend = ~mz,
                 yend = ~intensity, color = col, colors = colors, 
                 name = name,
                 hovertemplate = "<br>mz: %{x}<br>int: %{y}<br>")
}
