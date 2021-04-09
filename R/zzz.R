##' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
      paste("\nThis is SpectraVis version", packageVersion("SpectraVis"), "\n",
            " See `?SpectraVis` to get started.\n"))
}
