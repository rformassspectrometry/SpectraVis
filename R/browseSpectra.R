##' @importFrom PSMatch addFragments
##'
##' @import Spectra
.plot_spectrum <- function(sp, i) {
    n <- length(sp)
    sp <- sp[i] ## plotting a single spectrum
    ## title to contain MS level and retention time
    ttl <- paste0("MS", msLevel(sp),
                  " RT:", round(rtime(sp), 2), "s")
    ## if there's a sequence, add it to the title and label fragements
    if (!is.na(precursorMz(sp)))
        ttl <- paste0(ttl,
                      " Precuror m/z: ", round(precursorMz(sp), 2),
                      " z: ",precursorCharge(sp))
    if ("sequence" %in% spectraVariables(sp) && !is.na(sp$sequence)) {
        ttl <- paste0(ttl, " ", sp$sequence)
        lbls <- PSMatch::addFragments
    } else { ## else label peaks with their mz
        lbls <- function(z) {
            z <- peaksData(z)[[1L]]
            lbls <- format(z[, "mz"], digits = 4)
            k <- max(z[, "intensity"]) * 0.5
            lbls[z[, "intensity"] < k] <- ""
            lbls
        }
    }
    ## finish title with the current scan's index
    ttl <- paste0(ttl, " (", i, "/", n, ")")
    plotSpectra(sp, main = ttl,
                labels = lbls,
                labelCol = "steelblue")
}


##' @title Browse spectra in a Spectra object
##'
##' @description
##'
##' The `browseSpectra()` function opens a simple shiny application
##' that allows to browse trough the individual scans of a `Spectra`
##' object. The title of the scans contains the MS level, the
##' retention time, if available the precursor m/z and precursor
##' charge and a peptides sequence (as defined in the `sequence`
##' spectra variable) and the index of the scan. If a scans has a
##' sequence (as defined in the `sequence` spectra variable), the
##' matching fragments (as defined by `PSMatch::calculateFragments()` are
##' labelled, otherwise the most intense peaks (as defined as those
##' that have an intensity as high as half the highest peak) are
##' labelled with their m/z.
##'
##'See `?SpectraVis` for an example.
##'
##' @param object A non-empty instance of class `Spectra`.
##'
##' @return An object that represents the app.
##'
##' @export
##'
##' @import shiny
##'
##' @author Laurent Gatto
browseSpectra <- function(object) {
    stopifnot(inherits(object, "Spectra"))
    if (!length(object))
        stop("The 'Spectra' object is empty.")
    i <- 1
    ui <- fluidPage(
        fluidRow(
            column(1, actionButton("prv", "previous")),
            column(3, sliderInput("slider", "",
                                  min = 1, max = length(object), value = 1)),
            column(1, actionButton("nxt", "next"))
        ),
        plotOutput("plotSpectra")
    )

    server <- function(input, output, session) {
        ## start by displaying the first spectrum
        output$plotSpectra <- renderPlot(.plot_spectrum(object, i))
        ## update the plot if the slider is changed
        observeEvent(input$slider, {
            i <<- as.integer(input$slider)
            output$plotSpectra <- renderPlot(.plot_spectrum(object, i))
        })
        ## update the plot with the next spectrum
        observeEvent(input$nxt, {
            if (i < length(object)) i <<- i + 1
            output$plotSpectra <- renderPlot(.plot_spectrum(object, i))
        })
        ## update the plot with the previous spectrum
        observeEvent(input$prv, {
            if (i > 1) i <<- i - 1
            output$plotSpectra <- renderPlot(.plot_spectrum(object, i))
        })
    }
    shinyApp(ui, server)
}
