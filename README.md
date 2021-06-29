
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echarty

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/echarty)](https://cran.r-project.org/package=echarty)
[![Github version](https://img.shields.io/github/v/release/helgasoft/echarty?label=github)](https://github.com/helgasoft/echarty/releases)
[![R-CMD-check](https://github.com/helgasoft/echarty/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/helgasoft/echarty/actions/workflows/R-CMD-check.yaml)
[![size](https://img.shields.io/github/languages/code-size/helgasoft/echarty)](https://github.com/helgasoft/echarty/releases/)
<!--
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-day/echarty)](https://cranlogs.r-pkg.org/badges/last-day/echarty)   -->
<!-- badges: end -->

This package is a thin R wrapper around Javascript library
[ECharts](https://echarts.apache.org/en/index.html). The R list
parameters come directly from [ECharts’
documentation](https://echarts.apache.org/en/option.html). There are
just a few extra commands.  
Users can benefit from the **full functionality** of ECharts to build
interactive charts in R and Shiny with minimal coding.  
Wider connectivity and deployment potential through [crosstalk
support](https://helgasoft.github.io/echarty/xtalk.html).

## Installation

<!-- We recommend the 
Older release on -->

Latest development version:

``` r
# install.packages("remotes")
remotes::install_github("helgasoft/echarty")
```

From [CRAN](https://CRAN.R-project.org):

``` r
install.packages("echarty")
```

## Examples

``` r
library(echarty)

#  basic chart 2D
cars %>% ec.init()

#  chart with plugin 3D, will prompt for one-time installation
p <- iris %>% ec.init(load='3D')
p$x$opts$series <- list(list(type='scatter3D'))
p
```

## Get started

<br />

The [**WEBSITE**](https://helgasoft.github.io/echarty) has detailed
tutorials and tips.  
<br /> There are plenty of [**code
examples**](https://github.com/helgasoft/echarty/blob/main/R/examples.R)
included. The easiest way to run them in RStudio is to type
**?ec.examples** in the Console, then copy/paste any code from Help to
see the result.  
Or run all examples at once with **example(‘ec.examples’)** and they
will show in the Viewer.

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
echarty. Powered by ECharts.
</p>
