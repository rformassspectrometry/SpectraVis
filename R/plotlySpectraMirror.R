#' @title Interactive visualisation of a mirror plot
#'
#' @description
#'
#' The `plotlySpectraMirror` function creates an interactive *mirror plot*
#' comparing two spectra `x` and `y` with each other. Peaks with matching m/z
#' values (considering `ppm` and `tolerance`) are indicated with a point which
#' size can be specified with parameter `matchSize`.
#'
#' See also [plotSpectraMirror()] for a non-interactive version of this plot.
#'
#' @param matchSize `numeric(1)` defining the size of the point that will be
#'     used to indicate peaks in `x` and `y` with matching m/z.
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
#' @importFrom plotly add_trace
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
#'     yLabel = "target", xColor = "red", yColor = "blue")
plotlySpectraMirror <- function(x, y, xLabel = "", xColor = "#737373",
                                yLabel = "", yColor = "#737373", matchSize = 5,
                                ppm = 20, tolerance = 0) {
    stopifnot(inherits(x, "Spectra"))
    stopifnot(inherits(y, "Spectra"))
    p <- plot_ly()
    if (length(x) > 1 || length(y) > 1)
        stop("'x' and 'y' have to be of length 1")
    if (length(x))
        x_peaks <- as.data.frame(peaksData(x)[[1L]])
    else x_peaks <- data.frame(mz = numeric(), intensity = numeric(),
                               match = character())
    if (length(y))
        y_peaks <- as.data.frame(peaksData(y)[[1L]])
    else y_peaks <- data.frame(mz = numeric(), intensity = numeric(),
                               match = character())

    x_range <- range(x_peaks$mz, y_peaks$mz, na.rm = TRUE) + c(-1, 1)
    y_max <- max(x_peaks$intensity, y_peaks$intensity, na.rm = TRUE)
    y_peaks$intensity <- -y_peaks$intensity

    ht <- "<b>%{text}</b><br>mz: %{x}<br>int: %{y}"
    if (nrow(x_peaks)) {
        x_peaks$zero <- 0.0
        x_peaks$match <- ""
        x_peaks$color <- xColor[1L]
        idx <- which(common(x_peaks$mz, y_peaks$mz, tolerance, ppm))
        if (length(idx))
            x_peaks$match[idx] <- "matched"
        p <- .plotly_peaks(p, x_peaks, name = xLabel, col = xColor[1L],
                           hovertemplate = ht, text = ~match)
    }
    if (nrow(y_peaks)) {
        y_peaks$zero <- 0.0
        y_peaks$match <- ""
        y_peaks$color <- yColor[1L]
        idx <- which(common(y_peaks$mz, x_peaks$mz, tolerance, ppm))
        if (length(idx))
            y_peaks$match[idx] <- "matched"
        p <- .plotly_peaks(p, y_peaks, name = yLabel, col = yColor[1L],
                           hovertemplate = ht, text = ~match)
    }
    pks <- rbind(x_peaks, y_peaks)
    pks <- pks[pks$match != "", , drop = FALSE]
    if (nrow(pks))
        p <- add_trace(p, data = pks, x = ~mz, y = ~intensity,
                       type = "scatter", mode = "markers",
                       hoverinfo = "none", name = "matched",
                       marker = list(size = matchSize[1L],
                                     color = ~color))
    layout(p, xaxis = list(title = "m/z", zeroline = FALSE),
           yaxis = list(title = "intensity", zeroline = TRUE),
           hovermode = "x", hoverdistance = 1)
}
