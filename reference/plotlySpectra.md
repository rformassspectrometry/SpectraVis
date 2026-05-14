# Interactive visualisation of a single spectrum

The `plotlySpectra()` function displays a single spectrum stored in a
`Spectra` object using the
[`plotly::plot_ly()`](https://rdrr.io/pkg/plotly/man/plot_ly.html)
function.

See
[`?SpectraVis`](https://rformassspectrometry.github.io/SpectraVis/reference/SpectraVis.md)
for an example.

## Usage

``` r
plotlySpectra(object)
```

## Arguments

- object:

  A instance of class `Spectra` of length 1.

## Value

A `plotly` object.

## Author

Laurent Gatto, Johannes Rainer

## Examples

``` r
library(MsDataHub)
fl <- PestMix1_DDA.mzML()
#> see ?MsDataHub and browseVignettes('MsDataHub') for documentation
#> loading from cache
pest_ms2 <- filterMsLevel(Spectra(fl), 2L)

plotlySpectra(pest_ms2[950])

{"x":{"visdat":{"2f727957949d30":["function () ","plotlyVisDat"],"2f7279394bc34":["function () ","data"]},"cur_data":"2f7279394bc34","attrs":{"2f7279394bc34":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"xend":{},"yend":{},"type":"scatter","mode":"lines","line":{"color":"#737373"},"name":"","hovertemplate":"<br>mz: %{x}<br>int: %{y}<br>","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"m/z"},"yaxis":{"domain":[0,1],"automargin":true,"title":"intensity","zeroline":true},"hovermode":"x","hoverdistance":1,"showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[57.069438934326172,57.069438934326172,null,59.048797607421875,59.048797607421875,null,70.0648193359375,70.0648193359375,null,76.980316162109375,76.980316162109375,null,77.042373657226562,77.042373657226562,null,79.055419921875,79.055419921875,null,88.077919006347656,88.077919006347656,null,103.05307006835938,103.05307006835938,null,105.06984710693359,105.06984710693359,null,117.06050109863281,117.06050109863281,null,118.06696319580078,118.06696319580078,null,119.08686065673828,119.08686065673828,null,120.08188629150391,120.08188629150391,null,121.10140228271484,121.10140228271484,null,130.06497192382812,130.06497192382812,null,131.07473754882812,131.07473754882812,null,132.08108520507812,132.08108520507812,null,133.07806396484375,133.07806396484375,null,133.0888671875,133.0888671875,null,134.097412109375,134.097412109375,null,144.08026123046875,144.08026123046875,null,145.0889892578125,145.0889892578125,null,146.09689331054688,146.09689331054688,null,147.10331726074219,147.10331726074219,null,148.11289978027344,148.11289978027344,null,198.06719970703125,198.06719970703125,null,224.08460998535156,224.08460998535156,null,256.10723876953125,256.10723876953125,null,256.26263427734375,256.26263427734375],"y":[0,0.04452311247587204,null,0,0.015096173621714115,null,0,0.032888293266296387,null,0,0.077564731240272522,null,0,0.051730591803789139,null,0,0.11353815346956253,null,0,0.09218597412109375,null,0,0.059829182922840118,null,0,0.34233474731445312,null,0,0.021255265921354294,null,0,0.13875088095664978,null,0,0.064315140247344971,null,0,0.16145908832550049,null,0,0.02161901630461216,null,0,0.11202411353588104,null,0,0.31488606333732605,null,0,0.82731688022613525,null,0,0.022662853822112083,null,0,0.20397436618804932,null,0,0.136496901512146,null,0,0.15327748656272888,null,0,0.047326955944299698,null,0,0.13060009479522705,null,0,0.095308363437652588,null,0,5.6834721565246582,null,0,0.11059334129095078,null,0,6.7050633430480957,null,0,0.31439188122749329,null,0,0.47847124934196472],"type":"scatter","mode":"lines","line":{"color":"#737373"},"name":"","hovertemplate":["<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>",null,"<br>mz: %{x}<br>int: %{y}<br>","<br>mz: %{x}<br>int: %{y}<br>"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
