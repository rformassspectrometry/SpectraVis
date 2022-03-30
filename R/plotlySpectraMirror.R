#' @title Interactive visualisation of a mirror plot
#'
#' @description
#'
#' The `plotlySpectraMirror` function creates an interactive *mirror plot*
#' comparing two spectra `x` and `y` with each other. See also
#' [plotSpectraMirror()] for a non-interactive version of this plot.
#'
#' @param matchColor Color for matching peaks.
#' 
#' @param ppm `numeric(1)` with the m/z relative acceptable difference (in
#'     ppm) for peaks to be considered matching (see [common()] for more
#'     details).
#'
#' @param tolerance `numeric(1)` with the absolute acceptable difference of
#'     m/z values for peaks to be considered matching (see [common()] for more
#'     details).
#'
#' @param x A [Spectra()] object of length 1.
#'
#' @param xLabel Optional `character(1)` with the label (name) of `x`.
#'
#' @param xColor Color for peaks of spectrum `x`.
#' 
#' @param y A [Spectra()] object of length 1.
#'
#' @param yColor Color for peaks of spectrum `y`.
#'
#' @param yLabel Optional `character(1)` with the label (name) of `y`.
#'
#' @return A `plotly` object
#'
#' @author Johannes Rainer
#'
#' @importFrom MsCoreUtils common
#' 
#' @export
#'
#' @examples
#'
#' ## Load example data.
#' library(msdata)
#' fl <- system.file("TripleTOF-SWATH", "PestMix1_DDA.mzML", package = "msdata")
#' pest_ms2 <- filterMsLevel(Spectra(fl), 2L)
#'
#' plotlySpectraMirror(pest_ms2[948], pest_ms2[950])
#'
#' plotlySpectraMirror(pest_ms2[948], pest_ms2[959], xLabel = "query",
#'     yLabel = "target")
plotlySpectraMirror <- function(x, y, xLabel = "", xColor = "#737373",
                                yLabel = "", yColor = "#737373",
                                ppm = 20, tolerance = 0,
                                matchColor = "#80B1D3") {
    stopifnot(inherits(x, "Spectra"))
    stopifnot(inherits(y, "Spectra"))
    p <- plot_ly()
    p <- layout(p, xaxis = list(title = "m/z"),
                yaxis = list(title = "intensity", zeroline = TRUE),
                hovermode = "x", hoverdistance = 1)
    if (length(x) > 1 || length(y) > 1)
        stop("'x' and 'y' have to be of length 1")
    if (length(x))
        x_peaks <- as.data.frame(peaksData(x)[[1L]])
    else x_peaks <- data.frame(mz = numeric(), intensity = numeric())
    if (length(y))
        y_peaks <- as.data.frame(peaksData(y)[[1L]])
    else y_peaks <- data.frame(mz = numeric(), intensity = numeric())

    x_range <- range(x_peaks$mz, y_peaks$mz, na.rm = TRUE) + c(-1, 1)
    y_max <- max(x_peaks$intensity, y_peaks$intensity, na.rm = TRUE)
    y_peaks$intensity <- -y_peaks$intensity

    cols <- unique(c(xColor, yColor, matchColor))
    
    if (nrow(x_peaks)) {
        x_peaks$zero <- 0.0
        if (length(xColor) != nrow(x_peaks))
            xColor <- rep(xColor[1L], nrow(x_peaks))
        idx <- which(common(x_peaks$mz, y_peaks$mz,
                            tolerance = tolerance, ppm = ppm))
        if (length(idx)) xColor[idx] <- matchColor
        p <- .plotly_peaks(p, x_peaks, name = xLabel, col = xColor,
                           colors = cols)
    }
    if (nrow(y_peaks)) {
        y_peaks$zero <- 0.0
        if (length(yColor) != nrow(y_peaks))
            yColor <- rep(yColor[1L], nrow(y_peaks))
        idx <- which(common(y_peaks$mz, x_peaks$mz,
                            tolerance = tolerance, ppm = ppm))
        if (length(idx)) yColor[idx] <- matchColor
        p <- .plotly_peaks(p, y_peaks, name = yLabel, col = yColor,
                           colors = cols)
    }
    p
}
