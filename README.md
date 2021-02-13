
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Echarty

<!-- badges: start -->
<!-- badges: end -->

This package is a thin R wrapper around Javascript library
[Echarts.js](https://echarts.apache.org/en/index.html) v.5. The focus is
on simplicity and efficiency. The list parameters come directly from the
[original documentation](https://echarts.apache.org/en/option.html).
There are just a few extra commands. Users can build elaborate
interactive charts in R and Shiny with minimal coding.

## Installation

<!--
You can install the released version of Echarty from [CRAN](https://CRAN.R-project.org) with:
``` r
install.packages("echarty")
```
-->

Until approved by [CRAN](https://CRAN.R-project.org), you can install
the development version

``` r
# install.packages("remotes")
remotes::install_github("helgasoft/echarty")
```

## Example

``` r
library(echarty)

#  basic chart
cars %>% ec.init()

#  chart with plugins 3D and GL
e <- ec.init(load = c('3D','GL'))
e$x$opts$series[[1]] <- list(
 type = 'surface',
 data = ec.data(as.data.frame(as.table(volcano)), TRUE)
)
e 
```

There are plenty of code examples in the Help.  
Type **?ec.examples** in the Console, then copy/paste any example to see
the result.

Now you can start building [**beautiful
charts**](https://echarts.apache.org/examples/en/index.html) (and
[**more**](https://www.makeapie.com)) with R and Shiny!

 <br />
<p align="center">
<img src="man/figs/ssPolarStack.png" alt="Polar Stack" width="180"/>
<img src="man/figs/ssBars.gif" width="180"/>
<img src="man/figs/ssThemeRiver.png" width="180"/>
<img src="man/figs/ssBunny.gif" width="180"/> <br />
<img src="man/figs/ssVolcano.png" width="180"/>
<img src="man/figs/ssRose.png" width="180"/>
<img src="man/figs/ssGeomap.png" width="180"/>
<img src="man/figs/ssStackBar.png" width="180"/>
<br /><font size="1">Made with Echarty. Powered by Echarts.</font>
</p>
