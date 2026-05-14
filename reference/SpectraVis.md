# SpectraVis: Visualising and Exploring Spectra Data

This package defines a set of helper function to visualise and explore
mass spectrometry data stored as Spectra objects.

## SpectraVis functions

- plotlySpectra: Interactive visualisation of a single spectrum.

- browseSpectra: Browse spectra in a Spectra object.

## See also

Useful links:

- <https://rformassspectrometry.github.io/SpectraVis>

- Report bugs at
  <https://github.com/RforMassSpectrometry/SpectraVis/issues>

## Author

**Maintainer**: Laurent Gatto <laurent.gatto@uclouvain.be>
([ORCID](https://orcid.org/0000-0002-1520-2268))

Authors:

- Laurent Gatto <laurent.gatto@uclouvain.be>
  ([ORCID](https://orcid.org/0000-0002-1520-2268))

- Johannes Rainer <Johannes.Rainer@eurac.edu>
  ([ORCID](https://orcid.org/0000-0002-6977-7147))

## Examples

``` r
library(MsDataHub)
f <- MS3TMT10_01022016_32917.33481.mzML.gz()
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
sp <- Spectra(f)
sp
#> MSn data (Spectra) with 565 spectra in a MsBackendMzR backend:
#>       msLevel     rtime scanIndex
#>     <integer> <numeric> <integer>
#> 1           1   4422.62         1
#> 2           2   4422.65         2
#> 3           2   4422.67         3
#> 4           2   4422.74         4
#> 5           2   4422.80         5
#> ...       ...       ...       ...
#> 561         3   4493.77       561
#> 562         3   4493.96       562
#> 563         2   4493.98       563
#> 564         3   4494.14       564
#> 565         3   4494.34       565
#>  ... 34 more variables/columns.
#> 
#> file(s):
#> 31c07e7afc179a_10477

# if (interactive())
#   browseSpectra(sp)

## Use Ctrl+C to interrupt R and stop the application

# if (interactive())
#   plotlySpectra(sp[1])
```
