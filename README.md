
# <img src='man/figures/logo.png' width='70px' alt='' /> echarty

<!-- badges: start -->

[![R-CMD-check](https://github.com/helgasoft/echarty/workflows/R-CMD-check/badge.svg)](https://github.com/helgasoft/echarty/actions)
[![coverage](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg)](https://coveralls.io/github/helgasoft/echarty)
[![size](https://img.shields.io/github/languages/code-size/helgasoft/echarty)](https://github.com/helgasoft/echarty/releases/)
[![website](https://img.shields.io/badge/Website-Visit-blue)](https://helgasoft.github.io/echarty)
[![twitter](https://img.shields.io/twitter/follow/echarty.svg?style=social&label=Follow)](https://x.com/echarty_R)

<!--
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-day/echarty)](https://cranlogs.r-pkg.org/badges/last-day/echarty)   -->

<!-- badges: end -->

<a href='https://helgasoft.github.io/echarty'><img src="man/figures/echarty.gallery.png" alt="echarty.gallery" /></a>

This package is a thin R wrapper around Javascript library
[ECharts](https://echarts.apache.org/en/index.html).  
**One** major command(_ec.init_) uses R lists to support the [ECharts API](https://echarts.apache.org/en/option.html).  
Benefit from ECharts **full functionality** and build
interactive charts in R and Shiny with minimal overhead.  

Wider connectivity and deployment potential through [WebR](https://helgasoft.github.io/echarty/test/coder.html) and  [crosstalk](https://rpubs.com/echarty/crosstalk).  

<details> <summary><b>Compare to echarts4r</b> 📌</summary>

R package | echarts4r | echarty
--- | --- | ---
initial commit | Mar 12, 2018 | Feb 5, 2021
library size | ![875 KB](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg) | ![266KB](https://img.shields.io/github/languages/code-size/helgasoft/echarty)
test coverage | ![32%](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg) [![link](man/figures/external-link-16.png)](https://coveralls.io/github/JohnCoene/echarts4r) | ![98%](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg) [![link](man/figures/external-link-16.png)](https://coveralls.io/github/helgasoft/echarty)
lines of code | 1,202,681 [![link](man/figures/external-link-16.png)](https://api.codetabs.com/v1/loc/?github=JohnCoene/echarts4r) | 5,988 [![link](man/figures/external-link-16.png)](https://api.codetabs.com/v1/loc?github=helgasoft/echarty)
echarts.js version | 5.4.3 [![link](man/figures/external-link-16.png)](https://github.com/JohnCoene/echarts4r/blob/master/inst/htmlwidgets/lib/echarts-4.8.0/echarts-en.min.js) | 6.0.0 [![link](man/figures/external-link-16.png)](https://github.com/helgasoft/echarty/blob/main/inst/js/echarts.min.js)
API design | own commands with parameters | mostly [ECharts option](https://echarts.apache.org/en/option.html) lists <sup>(1)</sup>
number of commands | over [200](https://echarts4r.john-coene.com/reference/) | **one** command (ec.init) + optional utility commands
[dataset](https://echarts.apache.org/en/option.html#dataset) support | no | **yes**
[WebR](https://docs.r-wasm.org/webr/latest/) support | no	| **yes**
[crosstalk](https://rstudio.github.io/crosstalk/) support | no | **yes**
column-to-style | no	| **yes**
dependencies ([tools](https://www.rdocumentation.org/packages/tools/versions/3.6.2/topics/package_dependencies)) | 70 | 46 
dependencies ([WebR](https://repo.r-wasm.org)) | 188 | 46 

<!-- utilities | bezier, correlations, histogram, density, loess, flip, nesting, more | extended boxplots, tabsets, layouts, shapefiles, lotties, more -->
<sup>(1)</sup> We encourage users to follow the original ECharts API to construct charts with echarty. 
	This differs from echarts4r which uses own commands for most chart options.   

Comparison review done October 2025 for current versions of echarts4R and echarty.  
___
</details>

Please consider granting a Github star ⭐ to show your support.  

## Installation

<!-- [![Github version](https://img.shields.io/github/v/release/helgasoft/echarty?label=github)](https://github.com/helgasoft/echarty/releases)  <sup>.02</sup>  -->
Latest development build **1.7.0**

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
  timeline= list(autoPlay= TRUE),
  series.param = list(
    symbolSize= ec.clmn('Petal.Width', scale= 9),
    encode= list(x= 'Sepal.Width', y='Petal.Length'),
    markLine= list(data= list(list(type='max'), list(type='min')))
  )
)

# show a remote map chart, needs package leaflet installed
echarty::ec.fromJson('https://helgasoft.github.io/echarty/test/pfull.json')

```

## Get started

The **Coder** is a good introduction, type ```library(echarty); demo(coder)```.  
The [**WEBSITE**](https://helgasoft.github.io/echarty) has a vast gallery with code and tutorials.  
The package itself has [code examples](https://raw.githubusercontent.com/helgasoft/echarty/refs/heads/main/demo/examples.R)
included.
Now you can start building [**beautiful ECharts**](https://echarts.apache.org/examples/en/index.html) with R and Shiny!

<br>
<p align="center">
<a href='https://helgasoft.github.io/echarty/articles/gallery.html' target='_blank'>
  <img src="man/figures/ssPolarStack.png" alt="Polar Stack" width="180"/>
  <img src="man/figures/ssBars.gif" alt="timeline bars"/>
  <img src="man/figures/ssThemeRiver.png" alt="themeRiver" width="180"/>
  <img src="man/figures/ssBunny.gif" alt="3D"/> <br>
  <!-- img src="man/figures/ssMorph.gif" width="180"/ -->
  <img src="man/figures/ssRose.png" alt="rose pie" width="180"/>
  <img src="man/figures/ssSpeed.png" alt="map" width="180"/>
  <img src="man/figures/ssStackBar.png" alt="stacked grouped bars" width="180"/>
</a> 
<br>Made with echarty. Powered by ECharts.
</p>
