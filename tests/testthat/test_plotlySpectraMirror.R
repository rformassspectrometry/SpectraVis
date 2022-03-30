test_that("plotlySpectraMirror works", {
    expect_error(plotlySpectraMirror(3, pest_ms2[2]), "not TRUE")
    expect_error(plotlySpectraMirror(pest_ms2[2], 3), "not TRUE")
    expect_error(plotlySpectraMirror(pest_ms2, pest_ms2[1]), "length 1")
    expect_error(plotlySpectraMirror(pest_ms2[1], pest_ms2), "length 1")
    p <- plotlySpectraMirror(pest_ms2[1], pest_ms2[2])
    expect_true(is(p, "plotly"))

    ## One Spectra empty.
    p <- plotlySpectraMirror(pest_ms2[2], Spectra())
    expect_true(is(p, "plotly"))
    p <- plotlySpectraMirror(Spectra(), pest_ms2[2])
    expect_true(is(p, "plotly"))
})
