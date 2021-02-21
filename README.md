
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Echarty

<!-- badges: start -->
<!-- badges: end -->

This package is a thin R wrapper around Javascript library
[Echarts.js](https://echarts.apache.org/en/index.html) v.5. The focus is
on simplicity and efficiency. The R list parameters come directly from
[Echarts’ documentation](https://echarts.apache.org/en/option.html).
There are just a few extra commands. Users can build elaborate
interactive charts in R and Shiny with minimal coding.

## Installation

Released version from [CRAN](https://CRAN.R-project.org):

``` r
install.packages("echarty")     # v.0.1.0
```

We recommend the development version. It has important additions, see
[NEWS.md](NEWS.md).

``` r
# install.packages("remotes")
remotes::install_github("helgasoft/echarty")     # v.0.1.1
```

## Examples

``` r
library(echarty)

#  basic chart
cars %>% ec.init()

#  chart with plugin 3D, will prompt for one-time installation
e <- ec.init(load = '3D')
e$x$opts$series[[1]] <- list(
 type = 'surface',
 data = ec.data(as.data.frame(as.table(volcano)), TRUE)
)
e 
```

## Get help

There are plenty of code examples in RStudio Help.  
Type **?ec.examples** in the Console, then copy/paste any code from Help
to see the result.  
Or run all examples at once with **example(‘ec.examples’)** and they
will show in the Viewer.  
We are adding tutorials and tips on the
[website](https://helgasoft.github.io/echarty).  
Do not hesitate to ask questions in
[Discussions](https://github.com/helgasoft/echarty/discussions) or
report problems in
[Issues](https://github.com/helgasoft/echarty/issues).

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
<img src="man/figs/ssStackBar.png" width="180"/> <br />Made with
Echarty. Powered by Echarts.
</p>
