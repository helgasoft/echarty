
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figs/logo.png" width='70px' alt="" /> echarty

<!-- badges: start -->

[![R-CMD-check](https://github.com/helgasoft/echarty/workflows/R-CMD-check/badge.svg)](https://github.com/helgasoft/echarty/actions)
[![Coveralls test coverage](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg)](https://coveralls.io/r/helgasoft/echarty?branch=main)
[![size](https://img.shields.io/github/languages/code-size/helgasoft/echarty)](https://github.com/helgasoft/echarty/releases/)
[![website](https://img.shields.io/badge/Website-Visit-blue)](https://helgasoft.github.io/echarty)
<!--
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-day/echarty)](https://cranlogs.r-pkg.org/badges/last-day/echarty)   -->

<!-- badges: end -->

<a href='https://helgasoft.github.io/echarty'><img src="man/figs/echarty.gallery.png" alt="echarty.gallery" /></a>

This package is a thin R wrapper around Javascript library
[ECharts](https://echarts.apache.org/en/index.html).  
Two major commands use R lists to enclose the entire [ECharts API](https://echarts.apache.org/en/option.html). 
Users can benefit from ECharts **full functionality** to build
interactive charts in R and Shiny with minimal overhead.  

Wider connectivity and deployment potential through [crosstalk
support](https://helgasoft.github.io/echarty/xtalk.html).  


## Installation

<!-- [![Github version](https://img.shields.io/github/v/release/helgasoft/echarty?label=github)](https://github.com/helgasoft/echarty/releases)  <sup>.02</sup>  -->
Latest development build <strong>1.5.4.02</strong>

``` r
if (!requireNamespace('remotes')) install.packages('remotes')
remotes::install_github('helgasoft/echarty')
```

[![CRAN
status](https://www.r-pkg.org/badges/version/echarty)](https://cran.r-project.org/package=echarty) 
From [CRAN](https://CRAN.R-project.org):

``` r
install.packages('echarty')
```

## Examples

``` r
library(echarty); library(dplyr)

#  scatter chart (default)
cars |> ec.init()

#  parallel chart
ToothGrowth |> ec.init(ctype= 'parallel')

#  3D chart with GL plugin
iris |> group_by(Species) |> ec.init(load= '3D')

#  timeline of two series with grouping, formatting, autoPlay
iris |> group_by(Species) |> 
ec.init(
  tl.series= list(
    symbolSize= ec.clmn(4, scale=4),
    encode= list(x= NULL, y= c('Sepal.Width', 'Petal.Length')),
    markPoint= list(data= list(list(type='max'), list(type='min')))
  )
) |> ec.upd({
  timeline <- c(timeline, list(autoPlay= TRUE))
})

```

## Get started

The [**WEBSITE**](https://helgasoft.github.io/echarty) has a gallery with code and tutorials.  
<br /> The package has plenty of [**code
examples**](https://github.com/helgasoft/echarty/blob/main/R/examples.R)
included. Type
**?ec.examples** in the RStudio Console, then copy/paste any code from Help to
see the result.  

Now you can start building [**beautiful
ECharts**](https://echarts.apache.org/examples/en/index.html) (and
[**more**](https://www.makeapie.cn/echarts)) with R and Shiny!

<br />
<p align="center">
<a href='https://helgasoft.github.io/echarty/gallery.html' target='_blank'>
<img src="man/figs/ssPolarStack.png" alt="Polar Stack" width="180"/>
<img src="man/figs/ssBars.gif"/>
<img src="man/figs/ssThemeRiver.png" width="180"/>
<img src="man/figs/ssBunny.gif"/> <br />
<!-- img src="man/figs/ssMorph.gif" width="180"/ -->
<img src="man/figs/ssRose.png" width="180"/>
<img src="man/figs/ssSpeed.png" width="180"/>
<img src="man/figs/ssStackBar.png" width="180"/>
</a> 
<br />Made with echarty. Powered by ECharts.
</p>
