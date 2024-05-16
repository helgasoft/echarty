
# <img src='man/figs/logo.png' width='70px' alt='' /> echarty

<!-- badges: start -->

[![R-CMD-check](https://github.com/helgasoft/echarty/workflows/R-CMD-check/badge.svg)](https://github.com/helgasoft/echarty/actions)
[![coverage](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg)](https://coveralls.io/r/helgasoft/echarty?branch=main)
[![size](https://img.shields.io/github/languages/code-size/helgasoft/echarty)](https://github.com/helgasoft/echarty/releases/)
[![website](https://img.shields.io/badge/Website-Visit-blue)](https://helgasoft.github.io/echarty)
[![twitter](https://img.shields.io/twitter/follow/echarty.svg?style=social&label=Follow)](https://twitter.com/echarty_R)

<!--
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-day/echarty)](https://cranlogs.r-pkg.org/badges/last-day/echarty)   -->

<!-- badges: end -->

<a href='https://helgasoft.github.io/echarty'><img src="man/figs/echarty.gallery.png" alt="echarty.gallery" /></a>

This package is a thin R wrapper around Javascript library
[ECharts](https://echarts.apache.org/en/index.html).  
**One** major command(_ec.init_) uses R lists to support the [ECharts API](https://echarts.apache.org/en/option.html).  
Benefit from ECharts **full functionality** and build
interactive charts in R and Shiny with minimal overhead.  

Wider connectivity and deployment potential through [crosstalk](https://rpubs.com/echarty/crosstalk) and .  

<details> <summary><b>Compare to echarts4r</b> 📌</summary>

R package | echarts4r | echarty
--- | --- | ---
initial commit | Mar 12, 2018 | Feb 5, 2021
library size | ![878 KB](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg) | ![224KB](https://img.shields.io/github/languages/code-size/helgasoft/echarty)
test coverage | ![32%](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg) [![link](man/figs/external-link-16.png)](https://coveralls.io/github/JohnCoene/echarts4r) | ![93%](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg) [![link](man/figs/external-link-16.png)](https://coveralls.io/github/helgasoft/echarty)
lines of code | 1,202,623 [![link](man/figs/external-link-16.png)](https://api.codetabs.com/v1/loc/?github=JohnCoene/echarts4r) | 5,517 [![link](man/figs/external-link-16.png)](https://api.codetabs.com/v1/loc?github=helgasoft/echarty)
API design <sup>(1)</sup> | own commands with parameters | mostly [ECharts option](https://echarts.apache.org/en/option.html) lists
number of commands | over [200](https://echarts4r.john-coene.com/reference/) | **one** command + optional utility commands
data storage support | series data | **[datasets](https://echarts.apache.org/en/option.html#dataset)**, series data
dependencies ([packrat](https://rdrr.io/cran/packrat/src/R/recursive-package-dependencies.R#sym-recursivePackageDependencies)) | 65 | 40 
dependencies ([WebR](https://repo.r-wasm.org)) | 188 | 46 
[WebR](https://docs.r-wasm.org/webr/latest/) support | no	| **yes**
[crosstalk](https://rstudio.github.io/crosstalk/) support | no | **yes**
utilities | bezier, correlations, histogram, density, loess, flip, nesting, more | extended boxplots, tabsets, layouts, shapefiles, lotties, more

<sup>(1)</sup> We encourage users to follow the original ECharts API to construct charts with echarty. 
	This differs from echarts4r which uses own commands for most chart options.   

Comparison review done Feb 2024 for current versions of echarts4R and echarty.
</details>
  <br>
Please consider granting a Github star ⭐ to show your support.  

## Installation

<!-- [![Github version](https://img.shields.io/github/v/release/helgasoft/echarty?label=github)](https://github.com/helgasoft/echarty/releases)  <sup>.02</sup>  -->
Latest development build **1.6.3.03**

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
The package itself has also [**code examples**](https://github.com/helgasoft/echarty/blob/main/R/examples.R)
included. Type
**?ec.examples**, then copy/paste any code to
see the chart.  

Now you can start building [**beautiful
ECharts**](https://echarts.apache.org/examples/en/index.html) (and
[**more**](https://www.makeapie.cn/echarts)) with R and Shiny!

<br>
<p align="center">
<a href='https://helgasoft.github.io/echarty/gallery.html' target='_blank'>
  <img src="man/figs/ssPolarStack.png" alt="Polar Stack" width="180"/>
  <img src="man/figs/ssBars.gif"/>
  <img src="man/figs/ssThemeRiver.png" width="180"/>
  <img src="man/figs/ssBunny.gif"/> <br>
  <!-- img src="man/figs/ssMorph.gif" width="180"/ -->
  <img src="man/figs/ssRose.png" width="180"/>
  <img src="man/figs/ssSpeed.png" width="180"/>
  <img src="man/figs/ssStackBar.png" width="180"/>
</a> 
<br>Made with echarty. Powered by ECharts.
</p>
