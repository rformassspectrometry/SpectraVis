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
##' library(MsDataHub)
##' f <- MS3TMT10_01022016_32917.33481.mzML.gz()
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
"_PACKAGE"
