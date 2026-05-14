# Browse spectra in a Spectra object

The `browseSpectra()` function opens a simple shiny application that
allows to browse trough the individual scans of a `Spectra` object. The
title of the scans contains the MS level, the retention time, if
available the precursor m/z and precursor charge and a peptides sequence
(as defined in the `sequence` spectra variable) and the index of the
scan. If a scans has a sequence (as defined in the `sequence` spectra
variable), the matching fragments (as defined by
[`PSMatch::calculateFragments()`](https://rdrr.io/pkg/ProtGenerics/man/protgenerics.html)
are labelled, otherwise the most intense peaks (as defined as those that
have an intensity as high as half the highest peak) are labelled with
their m/z.

See
[`?SpectraVis`](https://rformassspectrometry.github.io/SpectraVis/reference/SpectraVis.md)
for an example.

## Usage

``` r
browseSpectra(object)
```

## Arguments

- object:

  A non-empty instance of class `Spectra`.

## Value

An object that represents the app.

## Author

Laurent Gatto
