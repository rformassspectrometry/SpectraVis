##' @title Interactive visualisation of a single spectrum
##'
##' @description
##'
##' The `plotlySpectrum()` function displays a single spectrum stored
##' in a `Spectra` object using the `plotly::plot_ly()` function.
##'
##' See `?SpectraVis` for an example.
##'
##' @param object A instance of class `Spectra` of length 1.
##'
##' @return A `plotly` object.
##'
##' @importFrom plotly plot_ly add_segments
##'
##' @export
##'
##' @author Laurent Gatto
plotlySpectra <- function(object) {
    stopifnot(inherits(object, "Spectra"))
    if (length(object) != 1)
        stop("Object must be of length 1.")
    x <- data.frame(peaksData(object)[[1]])
    plot_ly(data = x,
            x = ~mz, y= ~zero,
            xend = ~mz, yend = ~intensity) %>%
        add_segments()
}
