## Run tests with devtools::test() or testthat::test_file to create snapshots

library(msdata)
fl <- system.file("TripleTOF-SWATH", "PestMix1_DDA.mzML", package = "msdata")
pest_ms2 <- filterMsLevel(Spectra(fl), 2L)
pest_ms2 <- pest_ms2[c(808, 809, 945:955)]

test_that(".plotly_peaks works", {
    pd <- as.data.frame(peaksData(pest_ms2[1L])[[1L]])
    pd$zero <- 0
    p <- plot_ly()
    p <- .plotly_peaks(p, pd)
    expect_true(is(p, "plotly"))
    ## doppelganger seems not to work for plotly
    ## expect_doppelganger("plain plotly_peaks", .plotly_peaks(p, pd))
})

test_that("plotlySpectra works", {
    p <- plotlySpectra(Spectra())
    expect_true(is(p, "plotly"))
    
    expect_error(plotlySpectra(pest_ms2), "length 1")
    p <- plotlySpectra(pest_ms2[1L])
    expect_true(is(p, "plotly"))    
})
