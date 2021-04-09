##' SpectraVis: Visualising and Exploring Spectra Data
##'
##' This package defines a set of helper function to visualise and
##' explore mass spectrometry data stored as Spectra objects.
##'
##' @section SpectraVis functions:
##'
##' - plotlySpectra: Interactive visualisation of a single spectrum.
##'
##' - browseSpectra: Browse spectra in a Spectra object.
##'
##' @docType package
##'
##' @name SpectraVis
##'
##' @examples
##'
##' f <- msdata::proteomics(pattern = "MS3TMT10", full.names = TRUE)
##' sp <- Spectra(f)
##' sp
##'
##' if (interactive())
##'    browseSpectra(sp)
##'
##' ## Use Ctrl+C to interrupt R and stop the application
##'
##' if (interactive())
##'    plotlySpectra(sp[1])
NULL
